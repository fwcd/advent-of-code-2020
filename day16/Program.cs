using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;

namespace day16
{
    struct Constraint
    {
        public readonly string Name;
        private readonly int start;
        private readonly int end;

        public Constraint(string name, int start, int end)
        {
            Name = name;
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
        public readonly List<Constraint> Constraints;
        public readonly List<int> Fields;

        public int ErrorRate
        {
            get
            {
                var constraints = Constraints;
                return Fields.Where(f => !constraints.Any(c => c.Check(f))).Sum();
            }
        }

        public bool IsValid
        {
            get => ErrorRate == 0;
        }

        public Ticket(List<Constraint> constraints, List<int> fields)
        {
            Constraints = constraints;
            Fields = fields;
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

        private static List<Constraint> parseConstraints(ref IEnumerator<string> lines)
        {
            Console.WriteLine("Parsing constraints");
            return lines
                .TakeWhile(l => !string.IsNullOrWhiteSpace(l))
                .Select(l => l.Split(":").Select(s => s.Trim()).ToList())
                .SelectMany(l => l[1]
                    .Split("or")
                    .Select(s => s.Trim().Split("-").Select(int.Parse).ToList())
                    .Select(r => new Constraint(l[0], r[0], r[1])))
                .ToList();
        }

        private static List<Ticket> parseTickets(ref IEnumerator<string> lines, List<Constraint> constraints)
        {
            Console.WriteLine("Parsing tickets");
            return lines
                .TakeWhile(l => !string.IsNullOrWhiteSpace(l))
                .Select(l => new Ticket(constraints, l.Split(",").Select(int.Parse).ToList()))
                .ToList();
        }

        private static List<List<string>> findFieldNames(List<Ticket> tickets)
        {
            var ticket = tickets.First();
            // First find the name choices for each field
            var fieldNames = ticket.Fields
                .Select((_, i) => ticket.Constraints
                    .GroupBy(c => c.Name)
                    .Where(cs => tickets.All(t => cs.Any(c => c.Check(t.Fields[i]))))
                    .Select(cs => cs.Key)
                    .ToList())
                .ToList();
            // Since fields with a single choice are guaranteed to
            // have that field name, we can safely eliminate this
            // choice from the other fields.
            var singles = new HashSet<string>();
            string single = null;
            while ((single = fieldNames.Where(cs => cs.Count == 1).Select(cs => cs.First()).Where(c => !singles.Contains(c)).FirstOrDefault()) != null) {
                fieldNames = fieldNames.Select(cs => cs.Count == 1 ? cs : cs.Where(c => c != single).ToList()).ToList();
                singles.Add(single);
            }
            return fieldNames;
        }

        static void Main(string[] args)
        {
            IEnumerator<string> input = File.ReadAllLines("resources/input.txt").ToList().GetEnumerator();
            var constraints = parseConstraints(ref input);
            input.Skip(1); // 'your ticket:'
            var myTicket = parseTickets(ref input, constraints).Single();
            input.Skip(1); // 'nearby tickets:'
            var nearbyTickets = parseTickets(ref input, constraints);

            var part1 = nearbyTickets.Select(t => t.ErrorRate).Sum();
            Console.WriteLine($"Part 1: {part1}");

            var fieldNames = findFieldNames(nearbyTickets.Where(t => t.IsValid).ToList());
            Console.WriteLine($"Field names: {string.Join(", ", fieldNames.Select(f => string.Join(":", f)))}");

            var part2 = fieldNames
                .Zip(myTicket.Fields)
                .Where(z => z.First.First().StartsWith("departure"))
                .Select(z => new BigInteger(z.Second))
                .Aggregate((n, m) => n * m);
            Console.WriteLine($"Part 2: {part2}");
        }
    }
}
