using Xunit;
using ToDo;

public class ProgramTest
{
    private StringWriter stringWriter;
    private StringReader stringReader;
    private string testTaskList = 
        "----------------------------------------" + Environment.NewLine +
        "1. Task 1" + Environment.NewLine +
        "2. Task 2" + Environment.NewLine +
        "----------------------------------------" + Environment.NewLine;

    public ProgramTest()
    {
        stringWriter = new StringWriter();
        Console.SetOut(stringWriter);
        Program.TaskList.Clear();
    }

    [Fact]
    public void ShowMenuAdd_ValidTask() {
        // Arrange
        stringReader = new StringReader("Task 1");
        Console.SetIn(stringReader);
        var expectedOutput = 
            "Enter the name of the task: " + Environment.NewLine +
            "Task registered successfully" + Environment.NewLine;

        // Act
        Program.ShowMenuAdd();

        // Assert
        var actualOutput = stringWriter.ToString();
        Assert.Equal(expectedOutput, actualOutput);
    }

    [Fact]
    public void ShowMenuRemove_NoTasks() {
        // Arrange
        var expectedOutput = "There are no tasks to delete" + Environment.NewLine;

        // Act
        Program.ShowMenuRemove();
        
        // Assert
        var actualOutput = stringWriter.ToString();
        Assert.Equal(expectedOutput, actualOutput);
    }

    [Fact]
    public void ShowMenuRemove_ValidTask() {
        // Arrange
        Program.TaskList.Add("Task 1");
        Program.TaskList.Add("Task 2");
        stringReader = new StringReader("1");
        Console.SetIn(stringReader);
        var expectedOutput = 
            "Enter the number of the task to remove: " + Environment.NewLine +
            testTaskList +
            "Task Task 1 deleted" + Environment.NewLine;

        // Act
        Program.ShowMenuRemove();

        // Assert
        var actualOutput = stringWriter.ToString();
        Assert.Equal(expectedOutput, actualOutput);
    }

    [Fact]
    public void ShowMenuRemove_InvalidTask() {
        // Arrange
        Program.TaskList.Add("Task 1");
        Program.TaskList.Add("Task 2");
        stringReader = new StringReader("3");
        Console.SetIn(stringReader);
        var expectedOutput = 
            "Enter the number of the task to remove: " + Environment.NewLine +
            testTaskList +
            "Number of task not allowed" + Environment.NewLine;

        // Act
        Program.ShowMenuRemove();

        // Assert
        var actualOutput = stringWriter.ToString();
        Assert.Equal(expectedOutput, actualOutput);
    }

    [Fact]
    public void ShowMenuConsult_NoTasks()
    {
        // Arrange
        var expectedOutput = "There are no tasks to perform" + Environment.NewLine;

        // Act
        Program.ShowMenuConsult();

        // Assert
        var actualOutput = stringWriter.ToString();
        Assert.Equal(expectedOutput, actualOutput);
    }

    [Fact]
    public void ShowMenuConsult_WithTasks()
    {
        // Arrange
        Program.TaskList.Add("Task 1");
        Program.TaskList.Add("Task 2");
        var expectedOutput = testTaskList;

        // Act
        Program.ShowMenuConsult();

        // Assert
        var actualOutput = stringWriter.ToString();
        Assert.Equal(expectedOutput, actualOutput);
    }
}

