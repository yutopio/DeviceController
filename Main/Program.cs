using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.IO.Ports;
using System.Linq;
using System.Threading;
using Microsoft.FSharp.Collections;
using Command = System.Tuple<Types.device, object[], int, int>;
using Device = Types.device;
using ExtProc = Types.extProc;
using Proc = Types.proc;
using ProcBody = Types.ProcBody;
using Timeline = System.Tuple<Types.device[], System.Tuple<Types.device, object[], int, int>[], int>;

class Program
{
    static void Main(string[] args)
    {
        args = new[] { @"..\..\Docs\sample.txt" };

        if (args.Length != 1)
            throw new ArgumentNullException("No file specified for the input.");

        // Load program using Grammar library.
        var loadedProgram = Grammar.Load(args[0]);
        var priorityDuration = Grammar.mainPriority;
        Grammar.Reset();

        // Lookup Main procedure.
        Proc entrypoint = null;
        bool epFound = false;
        try
        {
            entrypoint = loadedProgram["Main"] as Proc;
            epFound = true;
        }
        catch (InvalidCastException) { }
        catch (KeyNotFoundException) { }
        if (!epFound) throw new EntryPointNotFoundException("No entrypoint (Main procedure) found");

        // We will first pre-execute the procedures.
        var devices = new Dictionary<Device, DeviceInstance>();
        var timelines = new List<TimelineInvocation>();
        PrepareProc(entrypoint, devices, new Dictionary<Device, Device>(), timelines);

        Console.WriteLine("Setting up devices.");

        // Setup all devices at once.
        foreach (var dev in devices)
            if (dev.Value is SerialEquip) ((SerialEquip)dev.Value).Open();

        Console.WriteLine("Now ready to start.");

        // DANGEROUS
        // TODO: But I still have wonder it works or not... :(
        if (0 < priorityDuration && priorityDuration <= 2147483)
        {
            var proc = Process.GetCurrentProcess();
            var mainThread = Thread.CurrentThread;
            var t = new Thread(() =>
            {
                Thread.Sleep(priorityDuration * 1000);
                proc.PriorityClass = ProcessPriorityClass.Normal;
                mainThread.Priority = ThreadPriority.Normal;
            });
            t.IsBackground = true;
            t.Priority = ThreadPriority.Highest;
            t.Start();

            Process.GetCurrentProcess().PriorityClass = ProcessPriorityClass.AboveNormal;
            Thread.CurrentThread.Priority = ThreadPriority.AboveNormal;
        }

        // Start timelines.
        foreach (var timeline in timelines) InvokeTimeline(timeline);

        // Finalize all devices.
        foreach (var dev in devices)
            if (dev.Value is SerialEquip) ((SerialEquip)dev.Value).Close();

        Console.WriteLine("Finished");
    }

    static void PrepareProc(Proc proc, Dictionary<Device, DeviceInstance> devices, Dictionary<Device, Device> sub, List<TimelineInvocation> timelines)
    {
        // Check registered devices. If not exists, create a new instance for it.
        foreach (var dev in proc.Devices)
            if (!devices.ContainsKey(dev))
                devices.Add(dev, DeviceInstanceFactory.Create(dev));
        foreach (var elem in proc.Body)
        {
            if (elem.IsT)
            {
                // Record timeline.
                var t = (elem as ProcBody.T).Item;
                timelines.Add(new TimelineInvocation
                {
                    DeviceInstances = t.Item1
                        .Select(dev => new { dev, ins = devices[sub.ContainsKey(dev) ? sub[dev] : dev] })
                        .ToDictionary(x => x.dev, x => x.ins),
                    Commands = t.Item2,
                    Duration = t.Item3
                });
            }
            else if (elem.IsI)
            {
                var i = (elem as ProcBody.I).Item;
                var inv = i.Item1;
                if (inv is Device)
                {
                    // Device invocation outside the timeline will be converted to the timeline with one device invocation.
                    var dev = (Device)inv;
                    var devInstances = new Dictionary<Device, DeviceInstance>();
                    devInstances.Add(dev, devices[sub.ContainsKey(dev) ? sub[dev] : dev]);
                    timelines.Add(new TimelineInvocation
                    {
                        DeviceInstances = devInstances,
                        Commands = new[] { new Command((Device)inv, i.Item2.ToArray(), 0, 0) },
                        Duration = 0
                    });
                }
                else if (inv is Proc)
                    // Within the file, no substitution will happen.
                    PrepareProc((Proc)inv, devices, sub, timelines);
                else if (inv is ExtProc)
                {
                    var extProc = (ExtProc)inv;

                    // Execute device substitution rule. Old rules are also inherited to children.
                    var newSub = new Dictionary<Device, Device>(sub);
                    foreach (var subElem in extProc.deviceBind)
                        newSub.Add(subElem.Key, newSub.ContainsKey(subElem.Value) ? newSub[subElem.Value] : subElem.Value);

                    // Call the external procedure.
                    PrepareProc(extProc.external, devices, newSub, timelines);
                }
                else new InvalidOperationException();
            }
            else new InvalidOperationException();
        }
    }

    static void InvokeTimeline(TimelineInvocation timeline)
    {
        // If timeline contains no command, do nothing.
        if (timeline.Commands.Length == 0)
            return;

        var cmdNum = 0;
        var deviceDic = timeline.DeviceInstances;
        var commands = timeline.Commands;
        var next = timeline.Commands[0];
        var nextTime = next.Item3;

        // Initialize stopwatch.
        var w = new Stopwatch();
        w.Start();

        for (;;)
        {
            if (w.ElapsedMilliseconds < nextTime) continue;

            // Execute device command.
            deviceDic[next.Item1].Execute(next.Item2);

            // When finish all the commands, just get out of the loop.
            if (++cmdNum == commands.Length) break;
            nextTime = (next = timeline.Commands[cmdNum]).Item3;
        }

        w.Stop();
    }
}

abstract class DeviceInstance
{
    public abstract void Execute(params object[] args);
}

static class DeviceInstanceFactory
{
    public static DeviceInstance Create(Device dev)
    {
        var devName = dev.id.Item2;
        if (devName == "Print") return new Print();
        if (devName == "Wait") return new Wait();
        return new SerialEquip(dev);
    }
}

class SerialEquip : DeviceInstance
{
    SerialPort port;

    public SerialEquip(Device configuration)
    {
        port = new SerialPort(configuration.portName,
            configuration.baudRate, (Parity)configuration.parity,
            configuration.dataBits, (StopBits)configuration.stopBits);
    }

    public void Open() { port.Open(); }
    public void Close() { port.Close(); }

    public override void Execute(params object[] args)
    {
        if (args == null) throw new ArgumentNullException();
        if (args.Length != 1 || !(args[0] is string))
            throw new ArgumentException("Serial port connected equipment only takes one string argument");

        port.Write((string)args[0]);
    }
}

class Print : DeviceInstance
{
    public override void Execute(params object[] args)
    {
        Console.WriteLine(args);
    }
}

class Wait : DeviceInstance
{
    public override void Execute(params object[] args)
    {
        Thread.Sleep((int)args[0]);
    }
}

class TimelineInvocation
{
    public Dictionary<Device, DeviceInstance> DeviceInstances { get; set; }
    public Command[] Commands { get; set; }
    public int Duration { get; set; }
}
