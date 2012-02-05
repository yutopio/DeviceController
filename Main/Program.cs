using System;
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        if (args.Length != 1)
        {
            Console.WriteLine("No file specified for the input.");
            return;
        }

        var stream = new FileStream(args[0], FileMode.Open, FileAccess.Read);
        var reader = new StreamReader(stream);
    }
}
