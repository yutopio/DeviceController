using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.IO.Ports;
using System.Linq;
using System.Reflection;
using System.Threading;
using Microsoft.FSharp.Collections;
using Device = Types.device;
using Proc = Types.proc;
using ProcBody = Types.procBody;

class Program
{
    static void Main(string[] args)
    {
        args = new[] { @"..\..\Docs\wrap.txt" };

        if (args.Length != 1)
            throw new ArgumentNullException("No file specified for the input.");

        // Load program using Grammar library.
        var loadedProgram = Grammar.Load(args[0]);
        Grammar.Reset();

        Console.WriteLine("File loaded {0}", args[0]);

        // Lookup Main procedure.
        Types.proc entrypoint = null;
        try { entrypoint = loadedProgram.First(x => x.name == "Main") as Types.proc; }
        catch { Error.epNotFound(); }

        Console.WriteLine("Preparing timelines.");

        // We will first pre-execute the procedures.
        var devices = new Dictionary<Device, DeviceInstance>();
        var timelines = PrepareProc(entrypoint, devices, new Dictionary<Device,Device>()).ToList();

        Console.WriteLine("Setting up devices.");

        // Setup all devices at once.
        foreach (var dev in devices)
            if (dev.Value is SerialDevice) ((SerialDevice)dev.Value).Open();

        Console.WriteLine("Testing all commands.");

        foreach (var timeline in timelines)
            foreach (var command in timeline.Commands)
                command.Device.Test(command.Parameters);

        Console.WriteLine("Ready to start. Press [Enter].");
        Console.ReadLine();

        // If priority exeuction is set, trigger the thread to control the program priority.
        var priorityDuration = -1;
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

        Console.WriteLine("=====");

        // Start timelines.
        foreach (var timeline in timelines) InvokeTimeline(timeline);

        Console.WriteLine("=====");
        Console.WriteLine("Execution finished.");
        Console.WriteLine("Closing devices.");

        // Finalize all devices.
        foreach (var dev in devices)
            if (dev.Value is SerialDevice) ((SerialDevice)dev.Value).Close();

        Console.WriteLine("All finished. Press [Enter].");
        Console.ReadLine();
    }

    static IEnumerable<Timeline> PrepareProc(Proc proc,
        Dictionary<Device, DeviceInstance> devices,
        Dictionary<Device, Device> substitution)
    {
        Func<Device, DeviceInstance> ConvertDevice = x =>
        {
            var temp = substitution.ContainsKey(x) ? substitution[x] : x;
            if (!devices.ContainsKey(temp))
                devices.Add(temp, DeviceInstanceFactory.Create(temp));
            return devices[temp];
        };

        foreach (var elem in proc.body)
            if (elem.IsTime)
            {
                var t = elem as ProcBody.Time;
                yield return new Timeline
                {
                    Commands = t.Item1.Select(x =>
                        new Command
                        {
                            Device = ConvertDevice(x.Item1),
                            Parameters = x.Item2.ToArray(),
                            Start = x.Item3
                        }).ToArray(),
                    Duration = t.Item2
                };
            }
            else if (elem.IsInvoke)
            {
                var i = elem as ProcBody.Invoke;
                var target = i.Item1;
                if (target is Device)
                    // Device invocation outside the timeline will be converted to the timeline with one device invocation.
                    yield return new Timeline
                    {
                        Commands = new[] {
                            new Command {
                                Device = ConvertDevice((Device)target),
                                Parameters = i.Item2.ToArray(),
                                Start = 0
                            } },
                        Duration = 0
                    };
                else if (target is Proc)
                    // Within the file, no substitution will happen.
                    foreach (var ret in PrepareProc((Proc)target, devices, substitution))
                        yield return ret;
                else if (target is Types.extProc)
                {
                    var extProc = (Types.extProc)target;

                    // Execute device substitution rule.
                    var newSubstitution = new Dictionary<Device, Device>();
                    foreach (var subElem in extProc.deviceBind)
                        newSubstitution.Add(
                            subElem.Item1,
                            substitution.ContainsKey(subElem.Item2) ?
                                substitution[subElem.Item2] : subElem.Item2);

                    // Call the external procedure.
                    foreach (var ret in PrepareProc(extProc.external, devices, newSubstitution))
                        yield return ret;
                }
                else new InvalidOperationException();
            }
            else new InvalidOperationException();
    }

    static void InvokeTimeline(Timeline timeline)
    {
        // If timeline contains no command, do nothing.
        if (timeline.Commands.Length == 0)
            return;

        var i = 0;
        var commands = timeline.Commands;
        var next = commands[0];
        var nextTime = next.Start;

        // Initialize stopwatch.
        var w = new Stopwatch();
        w.Start();

        for (; ; )
        {
            if (w.ElapsedMilliseconds < nextTime) continue;

            // Execute device command.
            next.Device.Execute(next.Parameters);

            // When finish all the commands, just get out of the loop.
            if (++i == commands.Length) break;
            nextTime = (next = timeline.Commands[i]).Start;
        }

        // Wait until the timeline finishes.
        while (w.ElapsedMilliseconds < timeline.Duration)
            Thread.Sleep(timeline.Duration - (int)w.ElapsedMilliseconds);

        w.Stop();
    }
}

public abstract class DeviceInstance
{
    public abstract void Test(params object[] args);
    public abstract void Execute(params object[] args);
}

public static class DeviceInstanceFactory
{
    public const string Serial = "serial";

    public static DeviceInstance Create(Device device)
    {
        if (device.deviceType == "#")
        {
            if (device.name == "Print") return new Print();
            else if (device.name == "Wait") return new Wait();
            else throw new InvalidOperationException("No such system device: " + device.name);
        }
        else if (device.deviceType == Serial)
            return new SerialDevice(device);
        else Error.unknownDevType(device.deviceType);

        // Should not reach here.
        throw new InvalidOperationException();
    }
}

public class SerialDevice : DeviceInstance
{
    SerialPort port;

    internal SerialDevice(Types.device serialDevice)
    {
        Debug.Assert(serialDevice.deviceType == DeviceInstanceFactory.Serial,
            "Device type mismatch.");

        port = new SerialPort();

        // Apply device configuration.
        var props = typeof(SerialPort).GetProperties();
        foreach (var config in serialDevice.configuration)
        {
            var prop = props.FirstOrDefault(x => x.Name == config.Item1);
            if (prop == null)
                throw new ArgumentException("No such configurable property: " + config.Item1);
            try { prop.SetValue(port, config.Item2, null); }
            catch (ArgumentException)
            {
                var propValue = config.Item2;
                if (prop.PropertyType.IsEnum && config.Item2 is string)
                {
                    if (!prop.PropertyType.IsEnumDefined(config.Item2))
                        throw new ArgumentException(string.Format(
                            "{0} does not exists as the value of {1}",
                            config.Item2, prop.PropertyType.FullName),
                            config.Item1);

                    prop.SetValue(port, Enum.Parse(prop.PropertyType, (string)config.Item2), null);
                }
                else throw;
            }
            catch (TargetInvocationException exp) { throw exp.InnerException; }
        }
    }

    ~SerialDevice() { if (IsOpen) Close(); }

    public void Open() { port.Open(); }
    public bool IsOpen { get { return port.IsOpen; } }
    public void Close() { port.Close(); }

    public override void Test(params object[] args)
    {
        if (args == null) throw new ArgumentNullException();
        if (args.Length != 1 || !(args[0] is string))
            throw new ArgumentException("Serial port connected equipment only takes one string argument");

        if (!port.IsOpen) throw new InvalidOperationException("Serial port is not opened yet.");
    }

    public override void Execute(params object[] args)
    {
        // During execution, no error should occur.
        try { port.Write((string)args[0]); }
        catch (Exception exp) { /* TODO: Error notification? */ }
    }
}

class Print : DeviceInstance
{
    public override void Test(params object[] args)
    {
        if (args == null) throw new ArgumentNullException();
    }

    public override void Execute(params object[] args)
    {
        Console.Write("[{0:0.000}]\t", Environment.TickCount / 1000f);

        if (args[0] is string)
            Console.WriteLine(args[0] as string, args.Skip(1).ToArray());
        else
            Console.WriteLine(args);
    }
}

class Wait : DeviceInstance
{
    public override void Test(params object[] args)
    {
        if (args == null) throw new ArgumentNullException();
        if (args.Length != 1 || !(args[0] is int))
            throw new ArgumentException("Wait procedure only takes one integer argument");
    }

    public override void Execute(params object[] args)
    {
        Thread.Sleep((int)args[0]);
    }
}

struct Timeline
{
    public Command[] Commands { get; set; }
    public int Duration { get; set; }
}

struct Command
{
    public DeviceInstance Device { get; set; }
    public object[] Parameters { get; set; }
    public int Start { get; set; }
}
