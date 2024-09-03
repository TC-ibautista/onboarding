using SingleResponsability;

StudentRepository studentRepository = new();
ExportServiceCSV exportServiceCSV = new();
exportServiceCSV.ExportStudents(studentRepository.GetAll());
Console.WriteLine("Process completed!");

Console.WriteLine("Press any key to finish...");
Console.ReadKey();
