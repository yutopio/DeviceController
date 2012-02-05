using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Pipes;
using System.Linq;
using System.Threading;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;

class Loader
{
    int highPriorityDuration = -1;
    List<FileInfo> loaded;
    List<Tuple<List<Types.device>, Dictionary<string, Types.proc>>> programInfo;

    public void LoadFile(string fileName)
    {
        loaded = new List<FileInfo>();
        programInfo = new List<Tuple<List<Types.device>, Dictionary<string, Types.proc>>>();
        LoadFile(new FileInfo(fileName), new List<int>());
    }

    int LoadFile(FileInfo file, List<int> loadGraph)
    {
        var stream = new FileStream(file.FullName, FileMode.Open, FileAccess.Read);
        var reader = new StreamReader(stream);

        // Maintain the load graph. At the same time, add to loaded file list.
        // Note that this is necessary to prevent include loop.
        int fileID;
        loadGraph = new List<int>(loadGraph);
        loadGraph.Add(fileID = loaded.Count);
        loaded.Add(file);
        programInfo.Add(null);

        // Prepare some state variables for parsing.
        Parser.InitState();

        // We don't put the file input directly to the parser because we need to
        // preprocess pp-directives. Instead, we make a pipe in order to pass
        // normal lines to parser. Lines starting with '#' will be parsed by
        // preprocessor parser.
        var pipeOut = new AnonymousPipeServerStream(
            PipeDirection.Out, HandleInheritability.None);
        var pipeIn = new AnonymousPipeClientStream(
            PipeDirection.In, pipeOut.ClientSafePipeHandle);

        // Create a thread to run a parser.
        FSharpList<Types.proc> result = null;
        var wh = new ManualResetEventSlim(false);
        var t = new Thread(() => {
            result = Grammar.Parse(new StreamReader(pipeIn));
            wh.Set();
        });

        // I want to run the thread to be more high priority than myself. This
        // prevent the data race if load the other file at together. Note that
        // this behavior is valid that pipe read doesn't use spin wait.
        t.Priority = ThreadPriority.AboveNormal;
        t.IsBackground = true;
        t.Start();

        // Seek a file line by line.
        using (var writer = new StreamWriter(pipeOut))
        while (!reader.EndOfStream)
        {
            var line = reader.ReadLine().Trim();
            if (line[0] != '#')
            {
                // For normal lines, just pass to the parser.
                writer.WriteLine(writer);
                continue;
            }

            // Let the parser to read all the data.
            pipeOut.WaitForPipeDrain();

            // Starting with #, do a special process.
            var pp = Grammar.Preprocess(line);
            if (pp.IsDevice)    // Device definition
                NotifyDevice(pp as Types.preproc.Device);
            else if (pp.IsLoad) // Load external program
            {
                var load = pp as Types.preproc.Load;
                var target = new FileInfo(
                    Path.IsPathRooted(load.Item1) ? load.Item1 :
                        Path.Combine(file.Directory.FullName, load.Item1));
                var loadedID = loaded.IndexOf(target);

                // Validate if the file can properly be loaded.
                if (loadGraph.Contains(loadedID))
                    throw new ArgumentException("Tried to load already loaded file: " +
                        target.FullName + "\r\nThis will cause a load loop.");
                else if (loadedID == -1)
                {
                    t.Suspend();    // I know this is inaccurate, but no other way to do.

                    // Okay, now go.
                    Parser.SaveState();
                    loadedID = LoadFile(target, loadGraph);
                    Parser.RestoreState();

                    t.Resume();
                }

                // Solve device replacement.
                var rep = FSharpList<Tuple<int, int>>.Empty;
                foreach (var substitute in load.Item2)
                {
                    var newID = Parser.GetDevice(substitute.Item2);

                    var oldDev = programInfo[loadedID].Item1
                        .Select((Value, ID) => new { Value, ID })
                        .FirstOrDefault(x => x.Value.name == substitute.Item1);

                    if (oldDev == null)
                        // No such device to replace name. Just ignore.
                        ;
                    else
                        rep = FSharpList<Tuple<int, int>>.Cons(
                            new Tuple<int, int>(oldDev.ID, newID), rep);
                }

                // After loading the file, we add some references.
                foreach (var exProcs in programInfo[loadedID].Item2)
                {
                    // The procedures that aren't defined in referring file
                    // should be ignored.
                    try
                    {
                        var _ = exProcs.Value.location.Value;
                        continue;
                    }
                    catch
                    {
                        var ident = exProcs.Value.name;
                        foreach (var substitute in load.Item2)
                        {
                            if (substitute.Item1 == exProcs.Value.name)
                            {
                                ident = substitute.Item2;
                                break;
                            }
                        }

                        if (Parser.Procs.ContainsKey(ident))
                            throw new ApplicationException(
                                "Procedure " + ident + " is already defined.");
                        Parser.Procs.Add(ident, new Types.proc
                        {
                            defined = true,
                            name = ident,
                            location = FSharpOption<Tuple<int, string>>.Some(
                            new Tuple<int, string>(loadedID, exProcs.Value.name)),
                            deviceBind = rep
                        });
                    }
                }
            }
            else if (pp.IsPriority) // Priority declaration (DANGEROUS)
            {
                // Only in the loaded module, this pragma is valid.
                // Otherwise ignored.
                var duration = (pp as Types.preproc.Priority).Item;
                if (loadGraph.Count == 1)
                {
                    if (duration < -1) throw new ArgumentOutOfRangeException(
                        "Specification of high priority duration is invalid: " +
                        duration.ToString());
                    highPriorityDuration = duration;
                }
            }
        }

        // Wait till the normal parse ends.
        wh.Wait();
        wh.Dispose();

        // Check if we don't have any procedures that are not defined yet.
        foreach (var kv in Parser.Procs)
        {
            var proc = kv.Value;
            if (!proc.defined)
                throw new InvalidProgramException("Procedure " + proc.name + " not defined.");
        }

        // Finalize parsing the file.
        programInfo[fileID] = new Tuple<List<Types.device>, Dictionary<string, Types.proc>>(
            Parser.Devices, Parser.Procs);
        return fileID;
    }

    void NotifyDevice(Types.preproc.Device deviceDecl)
    {
        var device = new Types.device { name = deviceDecl.Item1 };
        Parser.AddDevice(device);

        // If device specification is defined, store it to
        // the variable as well.
        try
        {
            var deviceSpec = deviceDecl.Item2.Value;    // None for exception
            device.portName = deviceSpec.Item1;
            var deviceParams = deviceSpec.Item2;
            device.baudRate = deviceParams[0];
            if (deviceParams.Length >= 2) device.parity = deviceParams[1];
            if (deviceParams.Length >= 3) device.dataBits = deviceParams[2];
            if (deviceParams.Length >= 4) device.stopBits = deviceParams[3];
        }
        catch (NullReferenceException) { }
    }
}
