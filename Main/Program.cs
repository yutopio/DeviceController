using System;
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        Grammar.Parse(new StreamReader(new FileStream(@"..\..\Docs\sample.txt", FileMode.Open)));
        return;

        if (args.Length != 1)
        {
            Console.WriteLine("No file specified for the input.");
            return;
        }

        var exec = new Executor(args[0]);
        exec.Prepare();
    }
}
