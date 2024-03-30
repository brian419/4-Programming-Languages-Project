#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <vector>
#include <limits>

#include "reading.h"
#include "Employee.h"

using namespace std;

bool check(double value, string op, double threshold)
{
	if (op=="eq")
		return value == threshold;
	else if (op=="ne")
		return value != threshold;
	else if (op=="ge")
		return value >= threshold;
	else if (op=="le")
		return value <= threshold;
	else if (op=="gt")
		return value > threshold;
	else if (op=="lt")
		return value < threshold;
	return false;
}

void printEmployees(vector<Employee*> *employees, string op, double threshold)
{
	cout << fixed << setprecision(2); 
	for (unsigned int i=0; i<employees->size(); i++) {
		double amount=employees->at(i)->getEarning();
		if (check(amount, op, threshold)) {
			cout << employees->at(i)->getInfo() << endl;
			cout << "earned $" <<  amount << endl << endl;
		}
	}
}

int countEmployees(vector<Employee*> *employees, string op, double threshold)
{
	int count=0;
    for (unsigned int i=0; i<employees->size(); i++) {
		double amount=employees->at(i)->getEarning();
        if (check(amount, op, threshold)) count++;
    }
	return count;
}

int main(int argc, char *argv[])
{
	if (argc!=3 && argc!=5) {
		cout << "Usage: " << argv[0] << " empployee_file action" << endl;
		cout << "or" << endl;
		cout << "Usage: " << argv[0] << " empployee_file action operator threshold" << endl;
		cout << "\nValid actions: count print min max total avg" << endl;
		cout << "Valid operators: eq ne gt ge lt le" << endl;
		return 1;
	}

	// read the emplyee file
	vector<Employee*> *employees=readFrom(argv[1]);
	if (employees->size()==0) {
		cout << "There are no employees." << endl;
		return 2;
	}

	// check how many satisfies the condition
	string op="ge";
	double threshold=0;
	if (argc==5) {
		op=string(argv[3]);
        string s(argv[4]);
        stringstream str_stream(s);
        str_stream >> threshold;
	}
	int count=countEmployees(employees, op, threshold);
	if (count==0) {
		cout << "There are no employees satisfied the specified condition." << endl;
		return 3;
	}
		

	{
		string action(argv[2]);
		if (action=="count") {
			cout << "There are " << count << " employees.\n" << endl;
		}
		else if (action=="print") {
			printEmployees(employees, op, threshold);
		}
		else if (action=="min") {
			double min=numeric_limits<double>::max();
			// cout << "min=" << min << endl;
			for (unsigned int i=0; i<employees->size(); i++) {
				double amount=employees->at(i)->getEarning();
				if (check(amount, op, threshold) && amount<min) {
					min=amount;
				}
			}
			// cout << "min=" << min << endl;
			printEmployees(employees, "eq", min);
		}
    	else if (action=="max") {
            double max=-1;
            for (unsigned int i=0; i<employees->size(); i++) {
                double amount=employees->at(i)->getEarning();
                if (check(amount, op, threshold) && amount>max) {
                    max=amount;
                }
			}
        	printEmployees(employees, "eq", max);
    	}
    	else if (action=="total" || action=="avg") {
        	double total=0;
        	for (unsigned int i=0; i<employees->size(); i++) {
                double amount=employees->at(i)->getEarning();
				if (check(amount, op, threshold)) {
            		total += amount;
				}
        	}
			cout << fixed << setprecision(2); 
			if (action=="total")
				cout << "Total payment is $" << total << endl << endl;
			else
				cout << "Average payment per employee is $" << total/count << endl << endl;
		}
		else {
			cout << "Invalid action: " << action << endl;
			cout << "Valid actions: print count min max sum avg" << endl;
		}
	}

	// clean up
	for (unsigned int i=0; i<employees->size(); i++) {
		delete employees->at(i);
	}
	employees->clear();

	return 0;
}