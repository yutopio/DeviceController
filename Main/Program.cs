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

        var load = new Loader();
        load.LoadFile(args[0]);
    }
}
