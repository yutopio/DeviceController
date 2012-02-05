using System;
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        var expr = Grammar.Parse(Console.In);
    }
}
