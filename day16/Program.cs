using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace day16
{
    struct Ticket
    {
        private readonly List<Predicate<int>> constraints;
        private readonly List<int> fields;

        public bool IsValid
        {
            get
            {
                var constraints = this.constraints;
                return fields.All(f => constraints.All(c => c(f)));
            }
        }

        public Ticket(List<Predicate<int>> constraints, List<int> fields)
        {
            this.constraints = constraints;
            this.fields = fields;
        }
    }

    class Program
    {
        private static List<Predicate<int>> parseConstraints(IEnumerable<String> lines)
        {
            return lines
                .TakeWhile(l => !String.IsNullOrWhiteSpace(l))
                .Select(l => l.Split(":").Select(s => s.Trim()).ToList()[1])
                .SelectMany(s => s
                    .Split("or")
                    .Select(s => s.Trim().Split("-").Select(int.Parse).ToList())
                    .Select(r => (Predicate<int>) (i => i >= r[0] && i <= r[1])))
                .ToList();
        }

        private static List<Ticket> parseTickets(IEnumerable<String> lines, List<Predicate<int>> constraints)
        {
            return lines
                .TakeWhile(l => !String.IsNullOrWhiteSpace(l))
                .Select(l => new Ticket(constraints, l.Split(",").Select(int.Parse).ToList()))
                .ToList();
        }

        static void Main(string[] args)
        {
            var input = File.ReadAllLines("resources/input.txt");
            var constraints = parseConstraints(input);
            input.Skip(1); // 'your ticket:'
            var myTicket = parseTickets(input, constraints).First();
            input.Skip(1); // 'nearby tickets:'
            var nearbyTickets = parseTickets(input, constraints);

            Console.WriteLine(string.Join(", ", input));
        }
    }
}
