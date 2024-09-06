using System;
using System.Collections.Generic;

namespace ToDo
{
    public class Program
    {
        public static List<string> TaskList { get; set; } = new List<string>();

        static void Main(string[] args)
        {
            int menuOption = 0;

            do
            {
                menuOption = ShowMainMenu();
                switch (menuOption)
                {
                    case (int)MenuOption.AddTask:
                        ShowMenuAdd();
                        break;

                    case (int)MenuOption.RemoveTask:
                        ShowMenuRemove();
                        break;

                    case (int)MenuOption.ConsultTasks:
                        ShowMenuConsult();
                        break;

                    case (int)MenuOption.Exit:
                        Console.WriteLine("See you!");
                        break;

                    default:
                        Console.WriteLine("Option not allowed, try again");
                        break;
                }
            } while (menuOption != (int)MenuOption.Exit);
        }
        
        /// <summary>
        /// Shows the tasks options to redirect to the selected menu
        /// </summary>
        /// <returns> Returns option selected by user </returns>
        public static int ShowMainMenu()
        {
            Console.WriteLine("----------------------------------------");
            Console.WriteLine("Enter the option to perform: ");
            Console.WriteLine("1. New task");
            Console.WriteLine("2. Remove task");
            Console.WriteLine("3. Pending tasks");
            Console.WriteLine("4. Exit");

            string optionSelected = Console.ReadLine();
            return Convert.ToInt32(optionSelected);
        }

        public static void ShowMenuRemove()
        {
            try
            {
                if (TaskList.Count == 0) 
                {
                    Console.WriteLine("There are no tasks to delete");
                    return;
                }

                Console.WriteLine("Enter the number of the task to remove: ");
                ShowCurrentTasks();
                string taskToRemove = Console.ReadLine();
                int indexToRemove = Convert.ToInt32(taskToRemove) - 1;

                if (indexToRemove > (TaskList.Count - 1) || indexToRemove < 0)
                {
                    Console.WriteLine("Number of task not allowed");
                    return;
                }
                
                if (indexToRemove > -1 && TaskList.Count > 0)
                {
                    string taskRemoved = TaskList[indexToRemove];
                    TaskList.RemoveAt(indexToRemove);
                    Console.WriteLine("Task " + taskRemoved + " deleted");
                }
            }
            catch (Exception)
            {
                Console.WriteLine("An error occurred while deleting the task");
            }
        }

        public static void ShowMenuAdd()
        {
            try
            {
                Console.WriteLine("Enter the name of the task: ");
                string newTask = Console.ReadLine();
                TaskList.Add(newTask);
                Console.WriteLine("Task registered successfully");
            }
            catch (Exception)
            {
                Console.WriteLine("An error occurred while adding the task");
            }
        }

        public static void ShowMenuConsult()
        {
            if (TaskList == null || TaskList.Count == 0)
            {
                Console.WriteLine("There are no tasks to perform");
            } 
            else
            {
                ShowCurrentTasks();
            }
        }

        public static void ShowCurrentTasks()
        {
            Console.WriteLine("----------------------------------------");
            var indexTask = 1;
            TaskList.ForEach(task => Console.WriteLine((indexTask++) + ". " + task));
            Console.WriteLine("----------------------------------------");
        }

        private enum MenuOption
        {
            AddTask = 1,
            RemoveTask = 2,
            ConsultTasks = 3,
            Exit = 4
        }
    }
}
