using Xunit;
using ToDo;

public class ProgramTest
{
    [Fact]
    public void ShowMenuConsultTest()
    {
        // Arrange
        string expected = "There are no tasks to perform";

        // Act
        string result = "Program.ShowMenuConsult()";

        // Assert
        Assert.Equal(expected, result);
    }
}