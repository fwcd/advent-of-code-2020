using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace day16
{
    struct Constraint
    {
        private readonly String name;
        private readonly int start;
        private readonly int end;

        public Constraint(String name, int start, int end)
        {
            this.name = name;
            this.start = start;
            this.end = end;
        }

        public bool Check(int value)
        {
            return value >= start && value <= end;
        }
    }

    struct Ticket
    {
        private readonly List<Constraint> constraints;
        private readonly List<int> fields;

        public int ErrorRate
        {
            get
            {
                var constraints = this.constraints;
                return fields.Where(f => !constraints.Any(c => c.Check(f))).Sum();
            }
        }

        public Ticket(List<Constraint> constraints, List<int> fields)
        {
            this.constraints = constraints;
            this.fields = fields;
        }
    }

    static class Program
    {
        private static List<T> TakeWhile<T>(this IEnumerator<T> enumerator, Predicate<T> p)
        {
            var elems = new List<T>();
            while (enumerator.MoveNext() && p(enumerator.Current))
            {
                // Console.WriteLine($"Taking {enumerator.Current}");
                elems.Add(enumerator.Current);
            }
            return elems;
        }

        private static void Skip<T>(this IEnumerator<T> enumerator, int n) {
            for (int i = 0; i < n; i++)
            {
                enumerator.MoveNext();
            }
        }

        private static List<Constraint> parseConstraints(ref IEnumerator<String> lines)
        {
            Console.WriteLine("Parsing constraints");
            return lines
                .TakeWhile(l => !String.IsNullOrWhiteSpace(l))
                .Select(l => l.Split(":").Select(s => s.Trim()).ToList())
                .SelectMany(l => l[1]
                    .Split("or")
                    .Select(s => s.Trim().Split("-").Select(int.Parse).ToList())
                    .Select(r => new Constraint(l[0], r[0], r[1])))
                .ToList();
        }

        private static List<Ticket> parseTickets(ref IEnumerator<String> lines, List<Constraint> constraints)
        {
            Console.WriteLine("Parsing tickets");
            return lines
                .TakeWhile(l => !String.IsNullOrWhiteSpace(l))
                .Select(l => new Ticket(constraints, l.Split(",").Select(int.Parse).ToList()))
                .ToList();
        }

        static void Main(string[] args)
        {
            IEnumerator<string> input = File.ReadAllLines("resources/input.txt").ToList().GetEnumerator();
            var constraints = parseConstraints(ref input);
            input.Skip(1); // 'your ticket:'
            var myTicket = parseTickets(ref input, constraints);
            input.Skip(1); // 'nearby tickets:'
            var nearbyTickets = parseTickets(ref input, constraints);

            var part1 = nearbyTickets.Select(t => t.ErrorRate).Sum();
            Console.WriteLine($"Part 1: {part1}");
        }
    }
}
