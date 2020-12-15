Imports System
Imports System.IO

Module Program
    Sub Main(args As String())
        Dim input As String() = File.ReadAllLines("resources/input.txt")
        Console.WriteLine(String.Join(", ", input))
    End Sub
End Module
