Imports System
Imports System.IO

Module Program
    Function DiffToPreviouslySpoken(spoken As Dictionary(Of Integer, Integer), i As Integer, v As Integer) As Integer
        Dim prev As Integer = 0
        Dim diff As Integer = 0
        If spoken.TryGetValue(v, prev) Then
            diff = i - prev
        End If
        Return diff
    End Function

    Function SequenceNth(startingNumbers As Integer(), n As Integer) As Integer
        Dim spoken As New Dictionary(Of Integer, Integer)
        Dim diffs As New Dictionary(Of Integer, Integer)
        Dim lastSpoken As Integer = 0
        Dim i As Integer = 0

        For Each startingNumber As Integer In startingNumbers
            diffs(startingNumber) = DiffToPreviouslySpoken(spoken, i, startingNumber)
            spoken(startingNumber) = i
            lastSpoken = startingNumber
            i += 1
        Next

        While i < n
            Dim diff As Integer = diffs(lastSpoken)
            Dim newDiff = DiffToPreviouslySpoken(spoken, i, diff)
            diffs(diff) = newDiff
            spoken(diff) = i
            lastSpoken = diff
            i += 1
        End While

        Return lastSpoken
    End Function

    Sub Main(args As String())
        Dim i As Integer = 0
        Dim inputLines As String() = File.ReadAllLines("resources/input.txt")
        Dim startingNumbers As Integer() = inputLines.Select(Function(raw)
            Dim n As Integer
            Return If(Integer.TryParse(raw, n), n, Nothing)
        End Function).Where(Function (n) Not IsNothing(n)).ToArray()

        Dim part1 As Integer = SequenceNth(startingNumbers, 2020)
        Console.WriteLine($"Part 1: {part1}")

        Dim part2 As Integer = SequenceNth(startingNumbers, 30000000)
        Console.WriteLine($"Part 2: {part2}")
    End Sub
End Module
