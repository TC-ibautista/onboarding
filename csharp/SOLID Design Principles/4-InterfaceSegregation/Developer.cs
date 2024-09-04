namespace InterfaceSegregation
{
    public class Developer : IDevelopActivities
    {
        public Developer()
        {
        }

        public void Develop() 
        {
            Console.WriteLine("I'm developing the functionalities required");
        }
    }
}