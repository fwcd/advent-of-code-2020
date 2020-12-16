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

        public int ErrorRate
        {
            get
            {
                var constraints = this.constraints;
                return fields.Where(f => !constraints.Any(c => c(f))).Sum();
            }
        }

        public Ticket(List<Predicate<int>> constraints, List<int> fields)
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

        private static List<Predicate<int>> parseConstraints(ref IEnumerator<String> lines)
        {
            Console.WriteLine("Parsing constraints");
            return lines
                .TakeWhile(l => !String.IsNullOrWhiteSpace(l))
                .Select(l => l.Split(":").Select(s => s.Trim()).ToList()[1])
                .SelectMany(s => s
                    .Split("or")
                    .Select(s => s.Trim().Split("-").Select(int.Parse).ToList())
                    .Select(r => (Predicate<int>) (i => i >= r[0] && i <= r[1])))
                .ToList();
        }

        private static List<Ticket> parseTickets(ref IEnumerator<String> lines, List<Predicate<int>> constraints)
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
            Console.WriteLine(string.Join(", ", part1));
        }
    }
}
