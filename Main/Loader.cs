using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Pipes;
using System.Linq;
using System.Threading;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;

partial class Executor
{
    int highPriorityDuration = -1;
    List<FileInfo> loaded;
    List<Tuple<List<Types.device>, Dictionary<string, Types.proc>>> programInfo;

    public Executor(string fileName)
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
            var line = reader.ReadLine();
            var temp = line.TrimStart();
            if (temp.Length == 0 || temp[0] != '#')
            {
                // For normal lines, just pass to the parser.
                // TODO: Line ending? Shouldn't we utilize binary operation?
                // TODO: A line starting with # inside verbatim string literal?
                writer.WriteLine(line);
                continue;
            }

            // Let the parser to read all the data.
            pipeOut.WaitForPipeDrain();

            // Starting with #, do a special process.
            var pp = Grammar.Preprocess(line);
            if (pp.IsDevice)    // Device definition
                NotifyDevice(pp as Types.preproc.Device);
            else if (pp.IsLoad) // Load external program
                LoadFile(pp as Types.preproc.Load, t, file, loadGraph);
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
        var device = new Types.device { defined = true, name = deviceDecl.Item1 };
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

    void LoadFile(Types.preproc.Load load, Thread parser, FileInfo file, List<int> loadGraph)
    {
        // Get the path to the file.
        var target = new FileInfo(Path.IsPathRooted(load.Item1) ? load.Item1 :
                Path.Combine(file.Directory.FullName, load.Item1));
        var loadedID = loaded.IndexOf(target);

        // Validate if the file can properly be loaded.
        if (loadGraph.Contains(loadedID))
            throw new ArgumentException("Tried to load already loaded file: " +
                target.FullName + "\r\nThis will cause a load loop.");
        else if (loadedID == -1)
        {
            parser.Suspend();    // I know this is inaccurate, but no other way to do.

            // Okay, now go.
            Parser.SaveState();
            loadedID = LoadFile(target, loadGraph);
            Parser.RestoreState();

            parser.Resume();
        }

        // Solve device name substitution.
        var substitutions = new Dictionary<string, string>();
        var deviceSub = FSharpList<Tuple<Types.device, Types.device>>.Empty;
        foreach (var substitute in load.Item2)
        {
            // e.g. "E1 as A"
            //       E1 ....... old name in the external file
            //             A .. new name in the current file
            var newDev = Parser.GetDevice(substitute.Item2);

            // Look up an external file's device dictionary, and get the first
            // device with the exact name.
            // TODO: Support override of multiple appearing device names?
            //       For example, "E1[0] as A" for first external "#device E1"
            var oldDev = programInfo[loadedID].Item1
                .FirstOrDefault(x => x.name == substitute.Item1);

            // If such the device exists to replace, keep it in the list. Note
            // that the old name may also refer to the external procedure name.
            // So not always have a corresponding device.
            if (oldDev != null)
                deviceSub = FSharpList<Tuple<Types.device, Types.device>>.Cons(
                    new Tuple<Types.device, Types.device>(oldDev, newDev), deviceSub);

            // By the way, we verify that the duplicate substituion not to
            // appear. Checking by list.
            try { substitutions.Add(substitute.Item1, substitute.Item2); }
            catch (ArgumentException)
            {
                throw new InvalidProgramException(
                    "Duplicate substitution for the name " + substitute.Item1);
            }
        }

        // Next we resolve procedure substitutions.
        foreach (var exProcs in programInfo[loadedID].Item2)
        {
            // The procedures that aren't defined in referring file should be
            // ignored. In this case, since location is None in F#, it will
            // throw an exception.
            try
            {
                var _ = exProcs.Value.origin.Value;
                continue;
            }
            catch (NullReferenceException) { }

            // We utilitze a substitution dictionary we've made earlier.
            var ident = exProcs.Value.name;
            if (substitutions.ContainsKey(ident))
                ident = substitutions[ident];

            // Set a procedure.
            Parser.SetExProc(ident, loadedID, exProcs.Value.name, deviceSub);
        }
    }
}
