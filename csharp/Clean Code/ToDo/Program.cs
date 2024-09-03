using System;
using System.Collections.Generic;

namespace ToDo
{
    internal class Program
    {
        public static List<string> TaskList { get; set; } = new List<string>();

        static void Main(string[] args)
        {
            const int addTask = 1;
            const int removeTask = 2; 
            const int consultTasks = 3; 
            const int exit = 4;
            int menuOption = 0;

            do
            {
                menuOption = ShowMainMenu();
                switch (menuOption)
                {
                    case addTask:
                        ShowMenuAdd();
                        break;

                    case removeTask:
                        ShowMenuRemove();
                        break;

                    case consultTasks:
                        ShowMenuConsult();
                        break;

                    case exit:
                        Console.WriteLine("See you!");
                        break;

                    default:
                        Console.WriteLine("Option not allowed, try again");
                        break;
                }
            } while (menuOption != exit);
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

        /// <summary>
        /// Allows the user to remove one task from the list
        /// </summary>
        /// <returns> void </returns>
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

        /// <summary>
        /// Allows the user to add one task to the list
        /// </summary>
        /// <returns> void </returns>
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

        /// <summary>
        /// Verify if there are tasks in the list and then show them
        /// </summary>
        /// <returns> void </returns>
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

        /// <summary>
        /// Show all the task of the list
        /// </summary>
        /// <returns> void </returns>
        public static void ShowCurrentTasks()
        {
            Console.WriteLine("----------------------------------------");
            var indexTask = 1;
            TaskList.ForEach(task => Console.WriteLine((indexTask++) + ". " + task));
            Console.WriteLine("----------------------------------------");
        }
    }
}
