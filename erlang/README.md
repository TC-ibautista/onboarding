# Practice Project: Clean Code and Best practices in Erlang

Welcome to the practice repository for Clean Code and best pracices principles in Erlang! In this project, you will focus on learning and applying fundamental concepts of Erlang programming language and best practices for writing clean code.

## Project Objectives

The main goal of this repository is to enhance your Erlang programming skills through understanding and applying the following aspects:

1. **Clean Code**: We will learn to write clean and readable code, following the guidelines and recommendations of Robert C. Martin in his book "Clean Code." We will explore techniques and practices to enhance the quality and maintainability of our code.

2. **Type spec and dialyzer usage**: We will delve into the roght use of records. Using records and maps makes programming easier; instead of remembering where a data item is stored in a complex data structure, we just use the name of the item and the system figures out where the data is stored.

3. **Cowboy Webserver**: We will learn and undesrtand the use and stages of the Cowboy webserver. Cowboy aims to provide a complete HTTP stack in a small code base. It is optimized for low latency and low memory usage, in part because it uses binary strings. Cowboy provides routing capabilities, selectively dispatching requests to handlers written in Erlang.

4. **Build Tools** We will learn about the tools to build the projects. These tools are listed here: Rebar3, Docker, Makefile. All of them represent an specific piece of the current stack that helps us to deployed our solution in the upper environments


## Repository Structure

The repository will be organized as follows:

- **`erlang/`**: This folder will contain the source files of our Erlang project. 
  

## Contributions

Contributions are welcome! If you'd like to participate in this project, you can follow these steps:

1. Clone this repository to your local.
2. Create a branch with a descriptive name for your contribution.
3. Make your changes and improvements in the branch.
4. Submit a pull request so that we can review your changes and merge them into the main project.

## Recommended Resources

Here are some resources that can help you understand and apply the concepts of Clean Code and SOLID Design Principles:

- Book: "Clean Code: A Handbook of Agile Software Craftsmanship" by Robert C. Martin.
- Book: "Learn You Some Erlang for Great Good!" : https://learnyousomeerlang.com/content
- Book: "Programming Erlang (2nd edition)" by Joe Armstrong


## Let's Get Started!

1. Download the .zip file of this repository.
2. Using your TigerConnect GitHub account, create a new private repository (your username will be the owner) and share this private repo with your team.
3. You have to copy the project you just downloaded to your personal repository. Please version all the same files and do not forget to add the hidden files (for example, the .gitignore file).
4. Create a branch with a descriptive name for your coding exercise. 
5. Create as many commits as possible and, please, DO NOT push your code into any public repository.
6. There is no such thing as a perfect and complete solution. Do as much as you can, in the best way possible, but Challenge Yourself!
7. Follow the principle “Progress over Perfection”. Iterate your solution as much as you want.


## Exercise Description

### Scenario
  - You have recently joined a healthcare technology company as a platform software developer. 
  - Your first task is to create an API that manages staff roles. 
  - The API must support basic HTTP methods and handle JSON requests containing information about the staff roles in the clinics where the application is used. 
  - The API will be used by healthcare providers to store and retrieve staff roles data efficiently

#### Step 1: Set Up the Project
- Prepare your environment with the files neeed to generate the project. It includes the makefile, dockerfile, rebar config  and the project folder structure. 
- Keep in mind that this solution needs to run in a docker container

#### Step 2: Define de record structure to support the following payload
- Typespec and dialyzer

```js
{
  "event_type": "Arrival",
  "data": {
    "first_name": "Adam",
    "gender": "M",
    "last_name": "Everyman",
    "mrn": "99991946",
    "organization": "GPXS87a6lEnkmzONMeR2qIVg"
  }
}
```
Eventbus kafka usage

#### Step 3: Set your cowboy server to handle and expose the CRUD operations such as GET/POST/PUT/DELETE http methods
- You need to create a new module that will handle the HTTP requests. In this step you also need to define the routes for your application.
- Take into consideration the following flowchart https://ninenines.eu/docs/en/cowboy/2.10/guide/rest_flowcharts/


#### Step 4: Build the controllers to handle he operations and store them in a data repository
- You need to create a new module that will handle the HTTP requests. In this step you also need to define the routes for your application.
- Take into consideration the following flowchart https://ninenines.eu/docs/en/cowboy/2.10/guide/rest_flowcharts/



####  Step 5: Testing the API
- The testing stage is divided in diferente levels of the development cycle, for that reason you should first 
- Create tests using EUnit & Common Test

- Once your cowboy server is up and running 
    1. Use tools like Postman to test the API endpoints with sample JSON requests.
    2. Verify that you can create , update and delete roles , and also retrieve current roles assigned


## Tracking the progress
- It is important to give visibility to the team on the progress of this exercise. 
- To track the progress of this exercise we have created a Kanban board in Trello, you can find it here 
- https://trello.com/invite/b/apUn55pK/ATTIf09fdb95a1994326dc2ae60fb1e60115EC3EE57C/onboard-platform-exercise
- In this board you will find the different stages we consider for a task to be implemented.
- Together with your buddy, you can write down the tasks necessary to successfully complete this exercise. 
- Don't forget to put the acceptance criteria in each one, plus you can only have one task in progress. 
- Create PR pior merge your changes , you should pick your buddy

## Prepare a presentation with the following items:
- Understanding of the problem – Everybody understands the problem differently, explain what your account is.
- Challenges & Learnings – What challenges did you face and what you learned from them?
- Design of your solution – Why you decided to implement this solution
- Working solution – Prepare examples to take us through your answer.


Let's embark on your journey to write more solid and clean code at Tiger Connect! Explore the sample code, experiment, and feel free to share your learnings and accomplishments in the repository. Together, we can improve our skills and create higher quality software.

Happy coding! 👨‍💻👩‍💻
