#include "reading.h"
#include "salariedEmployee.h"
#include "hourlyEmployee.h"
#include "comissionEmployee.h"
#include <fstream>
#include <sstream>
#include <string>

using namespace std;

vector<Employee*> *readFrom(string fileName) {
    vector<Employee*> *employees = new vector<Employee*>();
    ifstream file(fileName);

    if (!file) {
        return employees; 
    }

    string line;
    while (getline(file, line)) {
        istringstream iss(line);
        string type;
        string firstName, lastName;
        double salary, hoursWorked, hourlyRate, salesAmount, comissionRate, minimalWeeklyPay;

        iss >> type >> firstName >> lastName;

        if (type == "salaried") {
            iss >> salary;
            employees->push_back(new SalariedEmployee(firstName, lastName, salary));
        } else if (type == "hourly") {
            iss >> hoursWorked >> hourlyRate;
            employees->push_back(new HourlyEmployee(firstName, lastName, hoursWorked, hourlyRate));
        } else if (type == "commission") {
            iss >> salesAmount >> comissionRate >> minimalWeeklyPay;
            employees->push_back(new ComissionEmployee(firstName, lastName, salesAmount, comissionRate, minimalWeeklyPay));
        }
    }

    return employees;
}
