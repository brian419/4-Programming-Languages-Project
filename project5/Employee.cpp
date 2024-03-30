#include <sstream>
#include "Employee.h"

using namespace std;

Employee::Employee(const string& first, const string& last)
   : firstName(first), lastName(last)
{
}

void Employee::setFirstName(const string& first)
{
	firstName = first;
}

string Employee::getFirstName() const
{
	return firstName;
}

void Employee::setLastName(const string& last)
{
	lastName = last;
}

string Employee::getLastName() const
{
	return lastName;
}

string Employee::getInfo() const
{
   return getFirstName() + " " + getLastName();
}