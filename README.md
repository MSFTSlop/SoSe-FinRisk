# SoSe-FinRisk
Fin Risk Project 1

##Disclaimer

This code was heavily vibecoded. Be advised that the author may not have grasped
the full calculation of the used concepts. Instead he had too much fun overengineering
a simple assignment XD

This code is supposed to be executed in a certain order, is it damaged then 
the code may break. 

###What is the order

1. Analyze the model

2. Select your Calculation Core (GARCH or Linear)

3. Select your Scenario

3.1 First you have to execute the base (Number 1)

3.2 Then you have to execute the hedge (Number 2)

3.3 Last but not least the stresstest (Number 3)

If this order is violated, the switch-case and if-statemeents should automatically
stop the code.

###How to execute this program

Execute Lines 1 to 27 in one go.

Now switch to the console and insert your prefered calculation core (number)

Execute Lines 28 to 54 in one go.

Now switch to the console and select the project scenario.

Execute Lines 55 - 147 in one go.

###How to change the model

After one run execute line 27 to change your value through the console.

All other instructions remain unchanged.

###How to change the scenario

After one run execute line 55 to change your value through the console.

All other instructions unchanged
