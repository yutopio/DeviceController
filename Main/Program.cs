using System;
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        args = new[] { @"..\..\Docs\sample.txt" };

        if (args.Length != 1)
        {
            Console.WriteLine("No file specified for the input.");
            return;
        }

        Grammar.Load(args[0]);
    }
}
