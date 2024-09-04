namespace InterfaceSegregation
{
    public class Tester : ITestActivities
    {
        public Tester()
        {
        }

        public void Test() 
        {
            Console.WriteLine("I'm testing the application");
        }
    }
}