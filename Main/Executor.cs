using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

partial class Executor
{
    Dictionary<Types.proc, Procedure> procedures;
    Dictionary<Types.procBody.Time, Timeline> timelines;

    public void Prepare()
    {
        procedures = new Dictionary<Types.proc, Procedure>();
        timelines = new Dictionary<Types.procBody.Time, Timeline>();

        PrepareProcs();
        var ep = GetEntrypoint();
    }

    public void PrepareProcs()
    {
        // First we make empty procedures so that we can refer them.
        foreach (var file in programInfo)
        {
            var devices = file.Item1;
            var procs = file.Item2;
            foreach (var procKV in procs)
            {
                // We don't do anything with external procedures at this moment.
                if (procKV.Value.origin != null) continue;

                // Create a structure to put procedure body later.
                procedures.Add(procKV.Value, new Procedure { Devices = devices.ToArray() });
            }
        }

        // Body of these procedures are later converted.
        var convProcs = procedures.Keys.ToArray();

        // Then we link external procedures to instances we've made in previous.
        foreach (var file in programInfo)
        {
            var devices = file.Item1;
            var procs = file.Item2;
            foreach (var procKV in procs)
            {
                // We only process external procedures this time.
                var option = procKV.Value.origin;
                if (option == null) continue;

                // Link them.
                var origin = option.Value;
                procedures.Add(procKV.Value,
                    procedures[programInfo[origin.Item1].Item2[origin.Item2]]);
            }
        }

        // Then next we convert body of procedures.
        foreach (var proc in convProcs)
            PrepareProc(proc);
    }

    void PrepareProc(Types.proc proc)
    {
        var invokables = new List<IInvokable>();
        foreach (var elem in proc.body)
        {
            if (elem.IsTime)
                invokables.Add(PrepareTimeline(elem as Types.procBody.Time));
            else if (elem.IsProc)
            {
                var invoke = elem as Types.procBody.Proc;
                if (invoke.Item1 is Types.device)
                {
                    var dev = invoke.Item1 as Types.device;
                    var args = invoke.Item2.Select(x => Eval(x)).ToArray();
                    invokables.Add(new Timeline
                    {
                        Devices = new[] { dev },
                        Commands = new[] {
                            new DeviceInvoke {
                                Device = dev,
                                Parameter = args,
                                Start = 0,
                                End = 0
                            }
                        },
                        Duration = 0
                    });
                }
                else
                {
                    var p = invoke.Item1 as Types.proc;
                    invokables.Add(new ProcedureInvocation
                    {
                        Target = procedures[p],
                        DeviceSubstitution = p.deviceBind.ToArray()
                    });
                }
            }
        }

        procedures[proc].Invokables = invokables.ToArray();
    }

    Timeline PrepareTimeline(Types.procBody.Time time)
    {
        // Commands are sorted by start time, then by DeviceID.
        var commands = new SortedSet<DeviceInvoke>(new InvocationComparer());

        // Convert all timed commands into C# struct.
        var cursor = 0;
        foreach (var cmd in time.Item)
        {
            var dev = new DeviceInvoke
            {
                Device = cmd.Item1,
                Parameter = cmd.Item2.Select(x => Eval(x)).ToArray()
            };

            var timeSpecOpt = cmd.Item3;
            if (timeSpecOpt == null)
                dev.Start = dev.End = cursor;
            else
            {
                // Analyze time specification.
                var timeSpec = timeSpecOpt.Value;

                if (timeSpec.Item1 != null)
                    dev.Start = cursor = timeSpec.Item1.Value;

                // Calculate the end time of the command, and then after,
                // shift the time cursor.
                var endSpec = timeSpec.Item2;
                cursor = dev.End = endSpec.IsTo ? (endSpec as Types.endTime.To).Item :
                      (endSpec as Types.endTime.For).Item + cursor;
            }

            // Add to sorted set.
            commands.Add(dev);
        }

        // Verify that the timeline is correctly aligned by time.
        var blockedTime = new Dictionary<Types.device, int>();
        foreach (var cmd in commands)
        {
            if (!blockedTime.ContainsKey(cmd.Device))
                blockedTime.Add(cmd.Device, cmd.End);
            else
            {
                if (cmd.Start > cmd.End)
                    throw new ArgumentOutOfRangeException(string.Format(
                        "Invalid time specification: {0}ms - {1}ms", cmd.Start, cmd.End));

                if (cmd.Start < blockedTime[cmd.Device])
                    // TODO: Not friendly exception message (Device name?)
                    throw new ArgumentOutOfRangeException(string.Format(
                        "Overlapping command specification for device {0} at {1}ms - {2}ms",
                        cmd.Device.portName, cmd.Start, cmd.End));

                blockedTime[cmd.Device] = cmd.End;
            }
        }

        // Create a timeline instance. Even for empty one, we do.
        var ret = new Timeline
        {
            Devices = blockedTime.Keys.ToArray(),
            Commands = commands.ToArray(),
            Duration = blockedTime.Count == 0 ? 0 : blockedTime.Max(x => x.Value)
        };
        timelines.Add(time, ret);
        return ret;
    }

    object Eval(Types.expr x)
    {
        if (x.IsConst)
        {
            var literal = (x as Types.expr.Const).Item;
            if (literal.IsFloat) { return (literal as Types.literal.Float).Item; }
            else if (literal.IsString) { return (literal as Types.literal.String).Item; }
            else if (literal.IsValue)
            {
                // NYI: Variable support
                throw new NotImplementedException("Variable is not supported.");
            }
            else throw new InvalidOperationException();
        }
        else if (x.IsAdd)
        {
            var add = x as Types.expr.Add;
            var o1 = Eval(add.Item1);
            var o2 = Eval(add.Item2);

            // TODO: Are you serious??????
            if (o1 is float && o2 is float) return (float)o1 + (float)o2;
            else if (o1 is string && o2 is string) return (string)o1 + (string)o2;
            else if (o1 is float && o2 is string) return (float)o1 + (string)o2;
            else if (o1 is string && o2 is float) return (string)o1 + (float)o2;
            else throw new InvalidOperationException();
        }
        else throw new InvalidOperationException();
    }

    Procedure GetEntrypoint()
    {
        var mainProgram = programInfo[0];

        // Look for Main procedure.
        if (!mainProgram.Item2.ContainsKey("Main"))
            throw new EntryPointNotFoundException(
                "Main procedure not found in " + loaded[0].FullName);
        var entrypoint = mainProgram.Item2["Main"];

        // And it should not be defined outside the input file.
        if (entrypoint.origin != null)
            throw new EntryPointNotFoundException(
                "Main procedure should be defined in " + loaded[0].FullName);

        return procedures[entrypoint];
    }
}

public interface IInvokable { }

public class Procedure
{
    // TODO: This maybe redundant...
    public Types.device[] Devices { get; set; }
    public IInvokable[] Invokables { get; set; }
}

public class ProcedureInvocation : IInvokable
{
    public Procedure Target { get; set; }
    public Tuple<Types.device, Types.device>[] DeviceSubstitution { get; set; }
}

public class Timeline : IInvokable
{
    public Types.device[] Devices { get; set; }
    public DeviceInvoke[] Commands { get; set; }
    public int Duration { get; set; }
}

public struct DeviceInvoke
{
    public Types.device Device { get; set; }
    public object[] Parameter { get; set; }

    public int Start { get; set; }
    public int End { get; set; }
}

public class InvocationComparer : Comparer<DeviceInvoke>
{
    public override int Compare(DeviceInvoke x, DeviceInvoke y)
    {
        var timeDiff = x.Start - y.Start;
        return timeDiff != 0 ? timeDiff : x.Device.name.CompareTo(y.Device.name);
    }
}
