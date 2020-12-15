using System;
using System.IO;

namespace day16
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] input = File.ReadAllLines("resources/input.txt");
            Console.WriteLine(string.Join(", ", input));
        }
    }
}
