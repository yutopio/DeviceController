using System;

class Program
{
    static void Main(string[] args)
    {
        if (args.Length != 1)
        {
            Console.WriteLine("No file specified for the input.");
            return;
        }

        var exec = new Executor(args[0]);
        exec.Prepare();
    }
}
