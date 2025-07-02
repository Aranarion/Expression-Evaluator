#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <tinyexpr.h>
#include <math.h>

#define INVALID_COMMAND_LINE_ERROR 4
#define FILE_DOES_NOT_OPEN_ERROR 7
#define INVALID_VARIABLES_ERROR 12
#define DUPLICATE_VARIABLES_ERROR 6
#define FORMAT_BUFFER_SIZE 20
#define LOOP_COMMAS 3
#define MAX_VARIABLE_LENGTH 22
#define DEFAULT_SIG_FIGS 3
#define LINE_BUFFER 500
#define THIRD_INDEX 3
#define FOURTH_INDEX 4
#define FIFTH_INDEX 5
#define LOOP_LENGTH 6

/* Represents the collection of non-loop variables where each index corresponds
 * to one variable
 *
 * char** names: array of strings with name of variable
 * double* values: array of variable values
 * int size: number of entries in variables (includes variables that have been
 * transformed to loops) int converted: number of variables that have been
 * converted to loops
 */
typedef struct {
    char** names;
    double* values;
    int size;
    int converted;
} Variables;

/* Represents the collection of loops where each index corresponds to one loop
 *
 * char** names: array of strings with names of loops
 * double* currentValue: array of the current value of the loop variable
 * double* startingValue: array of the value the loop will start at if looped
 * double* increment: array of amount by which currentValue will increase by
 * when looped double* finalValue: array of values by which current will not
 * exceed if looped
 */
typedef struct {
    char** names;
    double* currentValue;
    double* startingValue;
    double* increment;
    double* endValue;
    int size;
} Loops;

/* Represents information found from the command line
 *
 * char* fileName: string of the name of a file to read if given
 * char** variableStrings: array of strings that have been identified following
 * --define on the command line char** loopsStrings: array of loopables that
 * have been identified following --loopable on the command line
 */
typedef struct {
    char* fileName;
    char** variableStrings;
    char** loopsStrings;
} Information;

int decode_loops_strings(Loops*, Information*, const int*, Variables*);
int extend_loops(Loops*, Variables*, char*, char*, char*, char*, int*);
int reallocate_loops(Loops*, char*, double, double, double);
int decode_variable_strings(Variables*, Information*, const int*);
int extend_variables(Variables*, char*, char*, int*);
int download_sig_figs(int, int, int*, char**);
int download_loops(int, int, int*, Information*, char**);
int download_variable(int, int, int*, Information*, char**);
int range_new_loop(Variables*, Loops*, char*, double, double, double, int*);
int range_allocate_loop(Loops*, char*, double, double, double, int*);
int print_variables(Variables*, Loops*, int*);

/* decode_loop_strings()
 *
 * Processes array of loopable strings that are stored in the Information
 * struct. Function extracts each component, ensuring the format is valid and
 * stores valid loopable variables in the Loops struct. Also checks and returns
 * error if duplicates are found
 *
 * Loops* loops: Pointer to the loops struct that will be populated by valid
 * loopables Information* information: Pointer to the information struct that
 * has loopable strings from the initial command line const int* numberLoops:
 * Pointer to the number of loops that were stored in the information struct
 * Variables* variables: Pointer to the Variables struct which is used to check
 * for duplicates
 *
 * Returns: 0 if all loops are valid
 * Returns: INVALID_VARIABLES_ERROR if loop string format is invalid
 * Returns: DUPLICATE_VARIABLES_ERROR if a loop variable name is already defined
 * as another loop or variable
 */
int decode_loops_strings(Loops* loops, Information* information,
        const int* numberLoops, Variables* variables)
{
    loops->names = (char**)malloc(sizeof(char*));
    loops->currentValue = (double*)malloc(sizeof(double));
    loops->startingValue = (double*)malloc(sizeof(double));
    loops->increment = (double*)malloc(sizeof(double));
    loops->endValue = (double*)malloc(sizeof(double));
    loops->size = 0;
    int duplicated = 0;
    for (int i = 0; i < *numberLoops; i++) {
        int countEquals = 0;
        if (information->loopsStrings[i] != NULL) {
            for (int m = 0; m < (int)strlen(information->loopsStrings[i]);
                    m++) {
                if (information->loopsStrings[i][m] == ',') {
                    countEquals++;
                }
            }
        }
        if (countEquals != LOOP_COMMAS) {
            return INVALID_VARIABLES_ERROR;
        }
        char* name = strtok(information->loopsStrings[i], ",");
        char* startingValueString = strtok(NULL, ",");
        char* incrementString = strtok(NULL, ",");
        char* endValueString = strtok(NULL, ",");
        int result = extend_loops(loops, variables, name, startingValueString,
                incrementString, endValueString, &duplicated);
        if (result != 0) {
            return result;
        }
    }
    if (duplicated) {
        return DUPLICATE_VARIABLES_ERROR;
    }
    return 0;
}

/* Extends the Loops struct by adding a new loop by validating the loopString
 * structure. Checks if variable name ahs already been used and if valid will
 * append new loop to Loops structure.
 *
 * Loops* loops: Pointer to the Loops struct that if valid will have loop added
 * to Variables* variables: pointer to the Variables struct, used to determine
 * if duplicate name char* name: name of the loop char* startingValueString:
 * String of the starting value of the loop char* incrementString: String of the
 * increment of the loop char* endValueString: String of the end value of the
 * loop int* duplicated: pointer to integer that flags if a duplicate name is
 * being used
 *
 * Returns: 0 upon success of INVALID_VARIABLES_ERROR if loop form is invalid
 */
int extend_loops(Loops* loops, Variables* variables, char* name,
        char* startingValueString, char* incrementString, char* endValueString,
        int* duplicated)
{
    if (name == NULL || startingValueString == NULL || incrementString == NULL
            || endValueString == NULL) {
        return INVALID_VARIABLES_ERROR;
    }
    int nameLength = strlen(name);
    if (nameLength < 1 || nameLength > MAX_VARIABLE_LENGTH) {
        return INVALID_VARIABLES_ERROR;
    }
    for (int k = 0; k < nameLength; k++) {
        if (!isalpha((unsigned char)name[k])) {
            return INVALID_VARIABLES_ERROR;
        }
    }
    char* tempExcess;
    double startValue, increment, endValue;
    startValue = strtod(startingValueString, &tempExcess);
    if (*tempExcess != '\0' || strlen(startingValueString) == 0) {
        return INVALID_VARIABLES_ERROR;
    }
    increment = strtod(incrementString, &tempExcess);
    if (*tempExcess != '\0' || strlen(incrementString) == 0) {
        return INVALID_VARIABLES_ERROR;
    }
    endValue = strtod(endValueString, &tempExcess);
    if (*tempExcess != '\0' || strlen(endValueString) == 0) {
        return INVALID_VARIABLES_ERROR;
    }
    if ((startValue < endValue && increment < 0)
            || (startValue > endValue && increment > 0) || (increment == 0)) {
        return INVALID_VARIABLES_ERROR;
    }
    for (int j = 0; j < loops->size; j++) {
        if (!strcmp(loops->names[j], name)) {
            *duplicated = 1;
        }
    }
    for (int j = 0; j < variables->size; j++) {
        if (!strcmp(variables->names[j], name)) {
            *duplicated = 1;
        }
    }
    reallocate_loops(loops, name, startValue, increment, endValue);
    return 0;
}

/* Expands the Loops struct by reallocating memory to add a new entry
 *
 * Loops* loops: Pointer to the loops struct to be updated
 * char* name: Name of the loop variable
 * double startValue: start value of the loop variable
 * double increment: increment of the loop variable
 * double endValue: end value of the loop variable
 *
 * Return: 0 upon success
 */
int reallocate_loops(Loops* loops, char* name, double startValue,
        double increment, double endValue)
{
    loops->size++;
    loops->names = (char**)realloc(
            (void*)loops->names, (loops->size) * sizeof(char*));
    loops->currentValue = (double*)realloc(
            (void*)loops->currentValue, (loops->size) * sizeof(double));
    loops->startingValue = (double*)realloc(
            (void*)loops->startingValue, (loops->size) * sizeof(double));
    loops->increment = (double*)realloc(
            (void*)loops->increment, (loops->size) * sizeof(double));
    loops->endValue = (double*)realloc(
            (void*)loops->endValue, (loops->size) * sizeof(double));
    loops->names[loops->size - 1] = strdup(name);
    loops->currentValue[loops->size - 1] = startValue;
    loops->startingValue[loops->size - 1] = startValue;
    loops->increment[loops->size - 1] = increment;
    loops->endValue[loops->size - 1] = endValue;
    return 0;
}

/* Parses variable strings from Information struct and if valid stores them in
 * Variables struct, able to detect duplicate and invalid variables.
 *
 * Variables* variables: Pointer to Variables struct where valid variables will
 * be stored Information* information: Pointer to the information struct which
 * contains the variable strings const int* numberVariables: Pointer to number
 * of variable strings to be processed
 *
 * Returns: 0 on success, INVALID_VARIABLES_ERROR for an invalid variable and
 * DUPLICATE_VARIABLES_ERROR if duplicate variables are observed.
 */
int decode_variable_strings(Variables* variables, Information* information,
        const int* numberVariables)
{
    variables->size = 0;
    variables->converted = 0;
    variables->names = (char**)malloc(sizeof(char*));
    variables->values = (double*)malloc(sizeof(double));
    int duplicated = 0;
    for (int i = 0; i < *numberVariables; i++) {
        int countEquals = 0;
        if (information->variableStrings[i] != NULL) {
            for (int m = 0; m < (int)strlen(information->variableStrings[i]);
                    m++) {
                if (information->variableStrings[i][m] == '=') {
                    countEquals++;
                }
            }
        }
        if (countEquals != 1) {
            return INVALID_VARIABLES_ERROR;
        }
        char* key = strtok(information->variableStrings[i], "=");
        char* valueString = strtok(NULL, "=");
        int result = extend_variables(variables, key, valueString, &duplicated);
        if (result != 0) {
            return result;
        }
    }
    if (duplicated) {
        return DUPLICATE_VARIABLES_ERROR;
    }
    return 0;
}

/*Checks validity of variable name and its value e.g. making sure name consists
 * of alphabetic characters and is appropriately sized, if valid appends to
 * Variables struct. Also able to detect duplicate variables
 *
 * Variables* variables: Pointer to Variables struct where valid variables are
 * stored char* key: Name of variable to be added char* valueString: String of
 * the value of the variable int* duplicated: Pointer to integer that flags if a
 * duplicate variable has been detected
 *
 * returns: 0 if success else INVALID_VARIABLES_ERROR if variable is invalid.
 */
int extend_variables(
        Variables* variables, char* key, char* valueString, int* duplicated)
{
    if (key == NULL || valueString == NULL) {
        return INVALID_VARIABLES_ERROR;
    }
    int keyLength = strlen(key);
    if (keyLength < 1 || keyLength > MAX_VARIABLE_LENGTH) {
        return INVALID_VARIABLES_ERROR;
    }
    for (int k = 0; k < keyLength; k++) {
        if (!isalpha((unsigned char)key[k])) {
            return INVALID_VARIABLES_ERROR;
        }
    }
    char* tempExcess;
    double value = strtod(valueString, &tempExcess);
    if (key != NULL && *tempExcess == '\0' && strlen(valueString) > 0) {
        for (int j = 0; j < variables->size; j++) {
            if (!strcmp(variables->names[j], key)) {
                *duplicated = 1;
            }
        }
        variables->size++;
        variables->names = (char**)realloc(
                (void*)variables->names, (variables->size) * sizeof(char*));
        variables->values = (double*)realloc(
                (void*)variables->values, (variables->size) * sizeof(double));
        variables->names[variables->size - 1] = strdup(key);
        variables->values[variables->size - 1] = value;
    } else {
        return INVALID_VARIABLES_ERROR;
    }
    return 0;
}

/*Parses the command line arguments and extracts variables, loops, significant
 * figures and file name
 *
 * int numberArguments: number of commandline arguments
 * char** arguments: array of command line argument strings
 * int* sigFigs: Pointer to integer that stores number of sig figs default is 3
 * if not specified Information* information: Pointer to the Information struct
 * where strings are stored int* numberVariables: Pointer to integer that counts
 * variable strings int* numberLoops: Pointer to integer that counts loop
 * strings
 *
 * Returns: 0 if success or INVALID_COMMAND_LINE_ERROR if format of command line
 * is invalid
 */
int download_command_line(int numberArguments, char** arguments, int* sigFigs,
        Information* information, int* numberVariables, int* numberLoops)
{
    *sigFigs = 0;
    information->fileName[0] = '\0';
    *numberVariables = 0;
    *numberLoops = 0;
    int i;
    for (i = 1; i < numberArguments; i++) {
        if (!(strcmp(arguments[i], "--define"))) {
            int result = download_variable(i, numberArguments, numberVariables,
                    information, arguments);
            if (result != 0) {
                return result;
            }
            i++;
        } else if (!(strcmp(arguments[i], "--loopable"))) {
            int result = download_loops(
                    i, numberArguments, numberLoops, information, arguments);
            if (result != 0) {
                return result;
            }
            i++;
        } else if (!(strcmp(arguments[i], "--significantfigures"))) {
            int result
                    = download_sig_figs(i, numberArguments, sigFigs, arguments);
            if (result != 0) {
                return result;
            }
            i++;
        } else if (i + 1 == numberArguments && strcmp(arguments[i], "")
                && (strlen(arguments[i]) < 2
                        || ('-' != arguments[i][0]
                                && '-' != arguments[i][1]))) {
            int length = strlen(arguments[i]);
            if (length > 0) {
                information->fileName
                        = (char*)realloc((void*)information->fileName,
                                (length + 1) * sizeof(char));
                strcpy(information->fileName, arguments[i]);
            }
        } else {
            return INVALID_COMMAND_LINE_ERROR;
        }
    }
    if (*sigFigs == 0) {
        *sigFigs = DEFAULT_SIG_FIGS;
    }

    return 0;
}

/*Parses and validates sig figs from the command line
 *
 * int i: index of string with significant figures
 * int numberArguments: number arguyments on command line
 * int* sigFigs: Pointer to integer that stores number of sig figs
 * char** arguments: array of strings given on the command line
 *
 * Returns 0 on success of INVALID_COMMAND_LINE_ERROR if command line format is
 * invalid
 */
int download_sig_figs(
        int i, int numberArguments, int* sigFigs, char** arguments)
{
    if ((i + 1 == numberArguments) || (*sigFigs != 0)) {
        return INVALID_COMMAND_LINE_ERROR;
    }
    if (strlen(arguments[i + 1]) == 1 && '2' <= arguments[i + 1][0]
            && '8' >= arguments[i + 1][0]) {
        *sigFigs = atoi(arguments[i + 1]);
    } else {
        return INVALID_COMMAND_LINE_ERROR;
    }
    return 0;
}

/* Parses, validates and stores a loop from command line as a string in
 * information
 *
 * int i: index of command line
 * int numberArguments: number of strings on command line
 * int* numberLoops: pointer to integer tracking number of loops
 * Information* information: pointer to information struct where loop strings
 * are stored char** arguments: array of command line strings
 *
 * Returns 0 on success of INVALID_COMMAND_LINE_ERROR if format is invalid
 */
int download_loops(int i, int numberArguments, int* numberLoops,
        Information* information, char** arguments)
{
    if (i + 1 == numberArguments) {
        return INVALID_COMMAND_LINE_ERROR;
    }
    if (strcmp(arguments[i + 1], "")) {
        (*numberLoops)++;
        information->loopsStrings
                = (char**)realloc((void*)information->loopsStrings,
                        (*numberLoops) * sizeof(char*));
        information->loopsStrings[*numberLoops - 1] = arguments[i + 1];
    } else {
        return INVALID_COMMAND_LINE_ERROR;
    }
    return 0;
}

/* Parses, validates and stores a variable from command line as a string in
 * information
 *
 * int i: index of string on command line
 * int numberArguments: number of strings on command line
 * int* numberVariables: pointer to integer tracking number of variables
 * Information* information: Pointer to information struct where variable
 * strings if valid will be stored char** arguments: command line strings
 *
 * Returns 0 on succes or INVALID_COMMAND_LINE_ERROR if format is invalid
 */
int download_variable(int i, int numberArguments, int* numberVariables,
        Information* information, char** arguments)
{
    if (i + 1 == numberArguments) {
        return INVALID_COMMAND_LINE_ERROR;
    }
    if (strcmp(arguments[i + 1], "")) {
        (*numberVariables)++;
        information->variableStrings
                = (char**)realloc((void*)information->variableStrings,
                        (*numberVariables) * sizeof(char*));
        information->variableStrings[*numberVariables - 1] = arguments[i + 1];
    } else {
        return INVALID_COMMAND_LINE_ERROR;
    }
    return 0;
}

/* Determines if file can be opened
 *
 * Information* information: pointer to information struct which contains file
 * name
 *
 * Returns 0 if success otherwise FILE_DOES_NOT_OPEN_ERROR
 */
int check_open_file(Information* information)
{
    FILE* file = fopen(information->fileName, "r");
    if (file == NULL) {
        return FILE_DOES_NOT_OPEN_ERROR;
    }
    fclose(file);
    return 0;
}

/* Parses a string expression from file or live command file and if valid
 * initialises a new loop
 *
 * char* expression A string to be determined if valid loop
 * Variables* variables A pointer to variables struct to determine if variable
 * name already exists Loops* loops: A pointer to Loops struct that holds loop
 * data int* sigFigs: A pointer to integer of number of sig figs to print for
 * doubles
 *
 * Returns 0 if success otherwise INVALID_VARIABLES_ERROR if expression is
 * invalid
 */
int range(char* expression, Variables* variables, Loops* loops, int* sigFigs)
{
    int countEquals = 0;
    for (int m = 0; m < (int)strlen(expression); m++) {
        if (expression[m] == ',') {
            countEquals++;
        }
    }
    if (countEquals != LOOP_COMMAS) {
        return INVALID_VARIABLES_ERROR;
    }
    char* name = strtok(expression, ",");
    char* startingValueString = strtok(NULL, ",");
    char* incrementString = strtok(NULL, ",");
    char* endValueString = strtok(NULL, ",");
    if (name == NULL || startingValueString == NULL || incrementString == NULL
            || endValueString == NULL) {
        return INVALID_VARIABLES_ERROR;
    }
    int nameLength = strlen(name);
    if (nameLength < 1 || nameLength > MAX_VARIABLE_LENGTH) {
        return INVALID_VARIABLES_ERROR;
    }
    for (int k = 0; k < nameLength; k++) {
        if (!isalpha((unsigned char)name[k])) {
            return INVALID_VARIABLES_ERROR;
        }
    }
    char* tempExcess;
    double startValue, increment, endValue;
    startValue = strtod(startingValueString, &tempExcess);
    if (*tempExcess != '\0' || strlen(startingValueString) == 0) {
        return INVALID_VARIABLES_ERROR;
    }
    increment = strtod(incrementString, &tempExcess);
    if (*tempExcess != '\0' || strlen(incrementString) == 0) {
        return INVALID_VARIABLES_ERROR;
    }
    endValue = strtod(endValueString, &tempExcess);
    if (*tempExcess != '\n' || strlen(endValueString) == 0) {
        return INVALID_VARIABLES_ERROR;
    }
    if ((startValue < endValue && increment < 0)
            || (startValue > endValue && increment > 0) || (increment == 0)) {
        return INVALID_VARIABLES_ERROR;
    }
    range_new_loop(
            variables, loops, name, startValue, increment, endValue, sigFigs);
    return 0;
}

/* Checks whether loop with given name already exists and updates it otherwise
 * calls for allocation of memory for new loop variable
 *
 * Variables* variables: A pointer to Variables struct that holds variables
 * Loops* loops: A pointer to Loops struct that holds loops
 * double startValue: Starting value of new loop
 * double increment: Incremement of new loop
 * double endvalue: End value of new loop
 * int* sigFigs: Pointer to int telling how many sig figs doubles should be
 * printed with
 *
 * Return 0 if success
 */
int range_new_loop(Variables* variables, Loops* loops, char* name,
        double startValue, double increment, double endValue, int* sigFigs)
{
    for (int j = 0; j < loops->size; j++) {
        if (!strcmp(loops->names[j], name)) {
            loops->currentValue[j] = startValue;
            loops->startingValue[j] = startValue;
            loops->increment[j] = increment;
            loops->endValue[j] = endValue;
            char format[FORMAT_BUFFER_SIZE];
            snprintf(format, sizeof(format), "%%.%dg", sigFigs[0]);
            // REF: Inspired by
            // REF: https://www.geeksforgeeks.org/snprintf-c-library/
            printf("%s = ", loops->names[j]);
            printf(format, loops->currentValue[j]);
            printf(" (");
            printf(format, loops->startingValue[j]);
            printf(", ");
            printf(format, loops->increment[j]);
            printf(", ");
            printf(format, loops->endValue[j]);
            printf(")\n");
            return 0;
        }
    }
    for (int j = 0; j < variables->size; j++) {
        if (!strcmp(variables->names[j], name)) {
            variables->names[j] = " ";
            variables->converted++;
            break;
        }
    }
    range_allocate_loop(loops, name, startValue, increment, endValue, sigFigs);
    return 0;
}

/* Allocates memory for new loop and initialises it with provided details
 *
 * Loops* loops: Pointer to Loops struct where new loop will be added
 * char* name: name of new loop
 * double startValue: starting value of new loop
 * double increment: increment of new loop
 * double endValue: end value of new loop
 * int* sigFigs: pointer to integer telling how many sig figs doubles should be
 * printed to
 *
 * Returns 0
 */
int range_allocate_loop(Loops* loops, char* name, double startValue,
        double increment, double endValue, int* sigFigs)
{
    loops->size++;
    loops->names = (char**)realloc(
            (void*)loops->names, (loops->size) * sizeof(char*));
    loops->currentValue = (double*)realloc(
            (void*)loops->currentValue, (loops->size) * sizeof(double));
    loops->startingValue = (double*)realloc(
            (void*)loops->startingValue, (loops->size) * sizeof(double));
    loops->increment = (double*)realloc(
            loops->increment, (loops->size) * sizeof(double));
    loops->endValue = (double*)realloc(
            (void*)loops->endValue, (loops->size) * sizeof(double));
    loops->names[loops->size - 1] = strdup(name);
    loops->currentValue[loops->size - 1] = startValue;
    loops->startingValue[loops->size - 1] = startValue;
    loops->increment[loops->size - 1] = increment;
    loops->endValue[loops->size - 1] = endValue;
    char format[FORMAT_BUFFER_SIZE];
    snprintf(format, sizeof(format), "%%.%dg", sigFigs[0]);
    printf("%s = ", loops->names[loops->size - 1]);
    printf(format, loops->currentValue[loops->size - 1]);
    printf(" (");
    printf(format, loops->startingValue[loops->size - 1]);
    printf(", ");
    printf(format, loops->increment[loops->size - 1]);
    printf(", ");
    printf(format, loops->endValue[loops->size - 1]);
    printf(")\n");
    return 0;
}

/* Prints result of expression evaluation with current loop variable's value
 *
 * double value: evaluated expression
 * int* sig_figs: pointer to number of sig figs to display doubles with
 * int loopVarIndex Index of the loop variable whose name and current value
 * should be printed Loops* loops: Pointer to the loops struct which contains
 * loops
 */
int loop_expression_print(
        double value, int* sigFigs, int loopVarIndex, Loops* loops)
{
    char format[FORMAT_BUFFER_SIZE];
    snprintf(format, sizeof(format), "%%.%dg", sigFigs[0]);
    printf("Result = ");
    printf(format, value);
    printf(" when %s = ", loops->names[loopVarIndex]);
    printf(format, loops->currentValue[loopVarIndex]);
    printf("\n");
    return 0;
}

/* Evaluates expression for @loop calls
 *
 * Loops* loops: Pointer to loops which contains loops
 * Variables* variables: Pointer the variables struct which contains address of
 * variables to be overridden if necessary char* expression A string
 * representation of maths expression to be converted int loopVarIndex: index of
 * loop variable used in expression int* sigFigs: Pointere to number of sig figs
 * to display doubles
 *
 * returns 1 if successful or 1 if error
 */
int loop_expression(Loops* loops, Variables* variables, char* expression,
        int loopVarIndex, int* sigFigs)
{
    te_variable tevars[variables->size + loops->size];
    double values[variables->size + loops->size];
    int index = 0;
    for (int i = 0; i < loops->size; i++) {
        if (strcmp(loops->names[i], " ") != 0) {
            (values[index]) = (loops->currentValue[i]);
            te_variable var = {.name = loops->names[i],
                    .address = &(values[index]),
                    .type = TE_VARIABLE,
                    .context = NULL};
            tevars[index] = var;
            (index)++;
        }
    }
    for (int i = 0; i < variables->size; i++) {
        if (strcmp(variables->names[i], " ") != 0) {
            values[index] = (variables->values[i]);
            te_variable var = {.name = variables->names[i],
                    .address = &(values[index]),
                    .type = TE_VARIABLE,
                    .context = NULL};
            tevars[index] = var;
            (index)++;
        }
    }
    int repetitions = 1
            + (int)floor((loops->endValue[loopVarIndex]
                                 - loops->startingValue[loopVarIndex])
                    / loops->increment[loopVarIndex]);
    for (int i = 0; i < repetitions; i++) {
        loops->currentValue[loopVarIndex] = loops->startingValue[loopVarIndex]
                + i * loops->increment[loopVarIndex];
        values[loopVarIndex] = loops->currentValue[loopVarIndex];
        int errPos;
        te_expr* expr = te_compile(expression, tevars, index, &errPos);
        if (expr) {
            double value = te_eval(expr);
            loop_expression_print(value, sigFigs, loopVarIndex, loops);
            te_free(expr);
        } else {
            return 1;
        }
    }
    return 0;
}

/* Sets up assigment for given variable or loop in @loop by cehcking if variable
 * exists if it doesnt it creates it
 *
 * char* expressionVariable: name of varaible to be made or found
 * int* varaibleIndex: A pointer to the index of the variable to be found or
 * created int* loopIndex: A pointer to index of loop to be found Variables*
 * varaibles: A pointer to variables struct that contains varaibles Loops*
 * loops: A pointer to loops struct that contains loops
 *
 * Return 0 if successful or 1 if expressionVariable is empty
 */
int loop_assignment_setup(char* expressionVariable, int* variableIndex,
        int* loopIndex, Variables* variables, Loops* loops)
{
    if (!strcmp(" ", expressionVariable)) {
        return 1;
    }
    for (int i = 0; i < variables->size; i++) {
        if (!strcmp(expressionVariable, variables->names[i])) {
            *variableIndex = i;
            break;
        }
    }
    for (int i = 0; i < loops->size; i++) {
        if (!strcmp(expressionVariable, loops->names[i])) {
            *loopIndex = i;
            break;
        }
    }
    if (*variableIndex == -1 && *loopIndex == -1) {
        variables->size++;
        variables->names = (char**)realloc(
                (void*)variables->names, (variables->size) * sizeof(char*));
        variables->values = (double*)realloc(
                (void*)variables->values, (variables->size) * sizeof(double));
        variables->names[variables->size - 1] = strdup(expressionVariable);
        variables->values[variables->size - 1] = 0;
        *variableIndex = variables->size - 1;
    }
    return 0;
}

/* prints result of assignment in loop for variable or loop
 *
 * double value: Computed value thats assigned
 * Loops* loops: Pointer to loop struct that contains loops
 * char* expressionVariable: Name of varaible or loop which is being assigned
 * Variables* variables: Pointer to variables struct which contains variables
 * int loopIndex: index of loop where assignment takes place or -1 if not in
 * loop int variableIndex: index of variables where assignment takes place or -1
 * if not in variables int* sigFigs: pointer to number sig figs to print doubles
 * to int loopVarIndex: Index of the loop variable being iterated over int i:
 * Current iteration of loop
 *
 * Returns 0 when successful
 */
int loop_print_assignment(double value, Loops* loops, char* expressionVariable,
        Variables* variables, int loopIndex, int variableIndex, int* sigFigs,
        int loopVarIndex, int i)
{
    if (variableIndex == -1) {
        loops->currentValue[loopIndex] = value;
    } else {
        variables->values[variableIndex] = value;
    }
    char format[FORMAT_BUFFER_SIZE];
    snprintf(format, sizeof(format), "%%.%dg", sigFigs[0]);
    printf("%s = ", expressionVariable);
    printf(format, value);
    printf(" when %s = ", loops->names[loopVarIndex]);
    printf(format,
            loops->startingValue[loopVarIndex]
                    + i * loops->increment[loopVarIndex]);
    printf("\n");
    return 0;
}

/* Executes the assignment within a loop evaluating expression and assigning its
 * variable to a variable or loop
 *
 * Variables* variables: A pointer to Variables struct containing the variabvles
 * int* sig_figs: Pointer to integer of how many sig figs to print doubles to
 * int loopIndex: Index of the loop where the assignment is occuring
 * int variableIndex: The index of variable to be assigned or -1 if its a loop
 * char* expressionVariable: Name of variable or loop beibng assigned
 * Loops* loops: Pointer to loops struct that contains loops
 * int loopVarIndex Index of the loop variable being iterated over
 * char* expressionExpression The expression to be evaluated and assigned
 *
 * Returns 0 if successful or 1 if error
 */
int loop_assignment(Variables* variables, int* sigFigs, int loopIndex,
        int variableIndex, char* expressionVariable, Loops* loops,
        int loopVarIndex, char* expressionExpression)
{
    int repetitions = 1
            + (int)floor((loops->endValue[loopVarIndex]
                                 - loops->startingValue[loopVarIndex])
                    / loops->increment[loopVarIndex]);
    for (int i = 0; i < repetitions; i++) {
        loops->currentValue[loopVarIndex] = loops->startingValue[loopVarIndex]
                + i * loops->increment[loopVarIndex];
        te_variable tevars[variables->size + loops->size];
        double values[variables->size + loops->size];
        int index = 0;
        for (int i = 0; i < loops->size; i++) {
            if (strcmp(loops->names[i], " ") != 0) {
                values[index] = loops->currentValue[i];
                te_variable var = {.name = loops->names[i],
                        .address = &(values[index]),
                        .type = TE_VARIABLE,
                        .context = NULL};
                tevars[index] = var;
                index++;
            }
        }
        for (int i = 0; i < variables->size; i++) {
            if (strcmp(variables->names[i], " ") != 0) {
                values[index] = variables->values[i];
                te_variable var = {.name = variables->names[i],
                        .address = &(values[index]),
                        .type = TE_VARIABLE,
                        .context = NULL};
                tevars[index] = var;
                index++;
            }
        }
        int errPos;
        te_expr* expr
                = te_compile(expressionExpression, tevars, index, &errPos);
        if (expr) {
            double value = te_eval(expr);
            loop_print_assignment(value, loops, expressionVariable, variables,
                    loopIndex, variableIndex, sigFigs, loopVarIndex, i);
            te_free(expr);
        } else {
            return 1;
        }
    }
    return 0;
}

/* Processes a @loop command executing a loop with expression or assigning a
 * variable / loop with a value
 *
 * char* line: the string representation of the entire loop command
 * Variables* variables: Pointer to variables struct that contains variables
 * Loops* loops: Pointer to loops struct that contains loops
 * int* sigFigs: Pointer to number of sig figs to print doubles to
 *
 * Return 0 uf succesyk or 1 if there is error in syntax
 */
int loop(char* line, Variables* variables, Loops* loops, int* sigFigs)
{
    strtok(line, " ");
    char* variableName = strtok(NULL, " ");
    char* expression = strtok(NULL, "");
    int loopVarIndex = -1;
    for (int i = 0; i < loops->size; i++) {
        if (!strcmp(loops->names[i], variableName)) {
            loopVarIndex = i;
            loops->currentValue[i] = loops->startingValue[i];
        }
    }
    if (loopVarIndex == -1) {
        return 1;
    }
    int numberEquals = 0;
    for (int i = 0; i < (int)strlen(expression); i++) {
        if (expression[i] == '=') {
            numberEquals++;
        }
    }
    if (numberEquals == 0) {
        int result = loop_expression(
                loops, variables, expression, loopVarIndex, sigFigs);
        if (result != 0) {
            return result;
        }
    } else if (numberEquals == 1) {
        char* expressionVariable = strtok(expression, "=");
        char* expressionExpression = strtok(NULL, "");
        expressionVariable = strtok(expressionVariable, " ");
        int variableIndex = -1;
        int loopIndex = -1;
        int result = loop_assignment_setup(expressionVariable, &variableIndex,
                &loopIndex, variables, loops);
        if (result != 0) {
            return result;
        }
        result = loop_assignment(variables, sigFigs, loopIndex, variableIndex,
                expressionVariable, loops, loopVarIndex, expressionExpression);
        if (result != 0) {
            return result;
        }
    } else {
        return 1;
    }
    return 0;
}

/* Detects and process a @range or @print in given line, executing appriopriuate
 * operatio9n
 *
 * char* line: The string containing the command to process
 * Variable* variable: A pointer to the Variables struct containing variables
 * Loops* loops: A pointer to the loop stsruct containing loops
 * int* sig_figs: Pointer to integer dezxceibing number of sig figs to display
 * doubles
 *
 * Returns 0 if succesful else 1
 */
int detect_range_print(
        char* line, Variables* variables, Loops* loops, int* sigFigs)
{
    char* testString = strdup(line);
    int testStart = 0;
    int testEnd = strlen(testString);
    while (isspace(testString[testStart]) && testStart < testEnd) {
        testStart++;
    }
    while (isspace(testString[testEnd]) && testEnd > testStart) {
        testEnd--;
    }
    memmove(testString, testString + testStart, testEnd - testStart + 1);
    // REF: Inspired by https://www.geeksforgeeks.org/memmove-in-cc/
    if (!strcmp(testString, "@print\n")) {
        print_variables(variables, loops, sigFigs);
        free(testString);
        return 1;
    }
    int spaceCounter = 0;
    for (int i = 0; i < (int)strlen(testString); i++) {
        if (testString[i] == ' ') {
            spaceCounter++;
        }
    }
    char* rangeTest = strtok(testString, " ");
    char* rangeExpression = strtok(NULL, " ");
    if (!strcmp(rangeTest, "@range") && spaceCounter == 1 && line[0] == '@') {
        int res = range(rangeExpression, variables, loops, sigFigs);
        if (res != 0) {
            fprintf(stderr,
                    "Error in command, expression or assignment "
                    "operation\n");
        }
        free(testString);
        return 1;
    }
    free(testString);
    return 0;
}

/* Detects and procces a loop command on given line executing necessary
 * operations if valid loop command is found
 *
 * char* line: the String containing the command to process
 * Variables* variables: A pointer to the variables struct which contains
 * variables Loops* loops: A pointer to the loops struct which contains loops
 * int* sigFigs: pointer to integer describing number of sig figs to print
 * doubles to
 *
 * Return 0 if succesful otherwise 1
 */
int detect_loops(char* line, Variables* variables, Loops* loops, int* sigFigs)
{
    char* testString = strdup(line);
    if ((int)strlen(testString) > LOOP_LENGTH && testString[0] == '@'
            && testString[1] == 'l' && testString[2] == 'o'
            && testString[THIRD_INDEX] == 'o' && testString[FOURTH_INDEX] == 'p'
            && testString[FIFTH_INDEX] == ' '
            && isalpha(testString[LOOP_LENGTH])) {
        int res = loop(testString, variables, loops, sigFigs);
        if (res != 0) {
            fprintf(stderr,
                    "Error in command, expression or assignment "
                    "operation\n");
        }
        free(testString);
        return 1;
    }
    free(testString);
    return 0;
}

/* Assigns a value to variable or loop and prints the result
 *
 * char* variableName: the name of the variable or loop to be assigned the value
 * int* finished: A pointer to an integer that will be set to 1 once assignment
 * is complete Variables* variables: Pointer to variables struct that contains
 * variables Loops* loops: Pointer to loops struct that contains loops int*
 * sigFigs: Pointer to integer describing number of sig figs to print doubles to
 * double value: The value to be assigned to the variabl;e or loop
 *
 * Return 0
 */
int download_assignment_print(char* variableName, int* finished,
        Variables* variables, Loops* loops, int* sigFigs, double value)
{
    for (int i = 0; i < variables->size; i++) {
        if (strcmp(variableName, variables->names[i]) == 0) {
            variables->values[i] = value;
            char format[FORMAT_BUFFER_SIZE];
            snprintf(format, sizeof(format), "%%.%dg", sigFigs[0]);
            printf("%s = ", variables->names[i]);
            printf(format, variables->values[i]);
            printf("\n");
            *finished = 1;
            break;
        }
    }
    for (int i = 0; i < loops->size; i++) {
        if (strcmp(variableName, loops->names[i]) == 0) {
            loops->currentValue[i] = value;
            char format[FORMAT_BUFFER_SIZE];
            snprintf(format, sizeof(format), "%%.%dg", sigFigs[0]);
            printf("%s = ", loops->names[i]);
            printf(format, loops->currentValue[i]);
            printf("\n");
            *finished = 1;
            break;
        }
    }
    return 0;
}

/* Assigns a value to a variable and prints the result
 *
 * Variables* variables: Pointer to variables struct which contains variables
 * char* variablename: Name of varaible or loop to be assigned
 * int* sigFigs: number of sig figs to print doubles to
 * double value: The value to be assigned to the varaible or loop
 *
 * Returns 0
 */
int download_allocate_variable(
        Variables* variables, char* variableName, int* sigFigs, double value)
{
    variables->size++;
    variables->names = (char**)realloc(
            (void*)variables->names, variables->size * sizeof(char*));
    variables->values = (double*)realloc(
            (void*)variables->values, variables->size * sizeof(double));
    variables->names[variables->size - 1] = strdup(variableName);
    variables->values[variables->size - 1] = value;
    char format[FORMAT_BUFFER_SIZE];
    snprintf(format, sizeof(format), "%%.%dg", sigFigs[0]);
    printf("%s = ", variables->names[variables->size - 1]);
    printf(format, variables->values[variables->size - 1]);
    printf("\n");
    return 0;
}

/* Evaluated an expression and assigns its result to a variable or loop
 *
 * Loops* loops: A pointer to loops struct which contains in loops
 * Variables* variable: A pointer to variables struct which contains variables
 * char* expression: A string containing the expression to be evaluated
 * char* variableName: The same of the variatable to which the expression's
 * result will be assigned
 *
 * Return 0 if succesful else 1
 */
int download_assignment(Loops* loops, Variables* variables, char* expression,
        char* variableName, int* sigFigs)
{
    te_variable tevars[variables->size + loops->size];
    double values[variables->size + loops->size];
    int index = 0;
    for (int i = 0; i < variables->size; i++) {
        if (strcmp(variables->names[i], " ") != 0) {
            values[index] = variables->values[i];
            te_variable var = {.name = variables->names[i],
                    .address = &(values[index]),
                    .type = TE_VARIABLE,
                    .context = NULL};
            tevars[index] = var;
            index++;
        }
    }
    for (int i = 0; i < loops->size; i++) {
        if (strcmp(loops->names[i], " ") != 0) {
            values[index] = loops->currentValue[i];
            te_variable var = {.name = loops->names[i],
                    .address = &(values[index]),
                    .type = TE_VARIABLE,
                    .context = NULL};
            tevars[index] = var;
            index++;
        }
    }
    int errPos;
    te_expr* expr = te_compile(expression, tevars, index, &errPos);
    int finished = 0;
    if (expr) {
        double value = te_eval(expr);
        te_free(expr);
        download_assignment_print(
                variableName, &finished, variables, loops, sigFigs, value);
        if (!finished) {
            download_allocate_variable(variables, variableName, sigFigs, value);
        }
    } else {
        fprintf(stderr,
                "Error in command, expression or assignment "
                "operation\n");
        return 1;
    }
    return 0;
}

/* Evaluated a mathematical expression using current values of varaibles and
 * loops
 *
 * Variables* variables: Pointer to variables struct whyich contains varaibles
 * Loops* loops: Pointer to loops struct which contains loops
 * char* line: A string representation of expression to be evaluated
 * int* sigFigs: pointer to Number of signifciant figures to print double to
 *
 * return 0
 */
int download_expression(
        Variables* variables, Loops* loops, char* line, int* sigFigs)
{
    te_variable tevars[variables->size + loops->size];
    double values[variables->size + loops->size];
    int index = 0;
    for (int i = 0; i < variables->size; i++) {
        if (strcmp(variables->names[i], " ") != 0) {
            values[index] = variables->values[i];
            te_variable var = {.name = variables->names[i],
                    .address = &(values[index]),
                    .type = TE_VARIABLE,
                    .context = NULL};
            tevars[index] = var;
            index++;
        }
    }
    for (int i = 0; i < loops->size; i++) {
        if (strcmp(loops->names[i], " ") != 0) {
            values[index] = loops->currentValue[i];
            te_variable var = {.name = loops->names[i],
                    .address = &(values[index]),
                    .type = TE_VARIABLE,
                    .context = NULL};
            tevars[index] = var;
            index++;
        }
    }
    int errPos;
    te_expr* expr = te_compile(line, tevars, index, &errPos);
    if (expr) {
        double res = te_eval(expr);
        char format[FORMAT_BUFFER_SIZE];
        snprintf(format, sizeof(format), "%%.%dg", sigFigs[0]);
        printf("Result = ");
        printf(format, res);
        printf("\n");
        te_free(expr);
    } else {
        fprintf(stderr,
                "Error in command, expression or assignment "
                "operation\n");
    }
    return 0;
}

/* Counts number of equals and # chaarcters in a line to disregard comments and
 * determine validity
 *
 * char* line A string representration of input to analyse
 * int* numberEquals: A pointer that stores number of equals signs
 *
 * Return 0 if succesful, 1 if line has a comment
 */
int download_setup(char* line, int* numberEquals)
{
    int length = strlen(line);
    for (int i = 0; i < length; i++) {
        if (line[i] == '=') {
            (*numberEquals)++;
        }
    }
    int numberHash = 0;
    for (int i = 0; i < length; i++) {
        if (line[i] == '#') {
            numberHash++;
        }
    }
    if (numberHash > 0) {
        return 1;
    }
    return 0;
}

/* Checks if given variable name is valid
 *
 * char* variableName: String representation of name of variable to check
 *
 * Return 0 if valid, else 1
 */
int download_assignment_check_valid(char* variableName)
{
    int length = strlen(variableName);
    int check = 0;
    for (int i = 0; i < length; i++) {
        if (!isalpha((unsigned char)variableName[i])) {
            check = 1;
        }
    }
    if (strlen(variableName) < 1
            || strlen(variableName) > MAX_VARIABLE_LENGTH) {
        check = 1;
    }
    if (check) {
        fprintf(stderr,
                "Error in command, expression or assignment "
                "operation\n");
        return 1;
    }
    return 0;
}

/* Processes and executes a live command line by reading from command line
 * detecting special command and handling variable assignments or expressions
 *
 * Variables* varaibles: A pointer to variable struct which contains variables
 * Loops* loops: A pointer to loops struct which contains loops
 * int* sigFigs: Pointer to number of sig figs to print doubles to
 *
 * Return 0
 */
int download_live_command_line(Variables* variables, Loops* loops, int* sigFigs)
{
    char line[LINE_BUFFER];
    if (fgets(line, sizeof(line), stdin) != NULL) {
        int numberEquals = 0;
        int result = download_setup(line, &numberEquals);
        if (result != 0) {
            return 0;
        }
        result = detect_range_print(line, variables, loops, sigFigs);
        if (result != 0) {
            return 0;
        }
        result = detect_loops(line, variables, loops, sigFigs);
        if (result != 0) {
            return 0;
        }
        if (numberEquals == 1) {
            char* variableName = strtok(line, "=");
            char* expression = strtok(NULL, "=");
            int start = 0;
            int end = strlen(variableName) - 1;
            while (isspace(variableName[start]) && start < end) {
                start++;
            }
            while (isspace(variableName[end]) && end > start) {
                end--;
            }
            memmove(variableName, variableName + start, end - start + 1);
            variableName[end - start + 1] = '\0';
            int result = download_assignment_check_valid(variableName);
            if (result != 0) {
                return 0;
            }
            result = download_assignment(
                    loops, variables, expression, variableName, sigFigs);
            if (result != 0) {
                return 0;
            }
        } else if (numberEquals == 0) {
            download_expression(variables, loops, line, sigFigs);
        }
    } else {
        return 1;
    }
    return 0;
}

/* Executes @print command printing all defined variables and loop variables
 *
 * Variables* variables: A pointer to variable struct which contains variables
 * Loops* loops: A pointer to loops struct which contains loops
 * int* sigFigs: A pointer to number of sig figs to print doubles to
 *
 * Return 0
 */
int print_variables(Variables* variables, Loops* loops, int* sigFigs)
{
    if (variables->size - variables->converted == 0) {
        printf("No variables were defined.\n");
    } else {
        printf("Variables:\n");
        for (int j = 0; j < variables->size; j++) {
            if (strcmp(variables->names[j], " ")) {
                char format[FORMAT_BUFFER_SIZE];
                snprintf(format, sizeof(format), "%%.%dg", sigFigs[0]);
                printf("%s = ", variables->names[j]);
                printf(format, variables->values[j]);
                printf("\n");
            }
        }
    }
    if (loops->size == 0) {
        printf("No loop variables were defined.\n");
    } else {
        printf("Loop variables:\n");
        for (int j = 0; j < loops->size; j++) {
            char format[FORMAT_BUFFER_SIZE];
            snprintf(format, sizeof(format), "%%.%dg", sigFigs[0]);
            printf("%s = ", loops->names[j]);
            printf(format, loops->currentValue[j]);
            printf(" (");
            printf(format, loops->startingValue[j]);
            printf(", ");
            printf(format, loops->increment[j]);
            printf(", ");
            printf(format, loops->endValue[j]);
            printf(")\n");
        }
    }
    return 0;
}

/* Reads a file processing each line interpreting it as commands or for
 * executing variable assignments, expressions, range printing or loop detection
 * etc
 *
 * Information* information: A pointer to information struct that contains file
 * name Variables* variables: A pointer to variables struct which contains
 * variables Loops* loops: A pointer to loops struct which contains loops int*
 * sigFigs: A pointer to number of sig figs to print doubles to
 *
 * return 0
 */
int download_file(Information* information, Variables* variables, Loops* loops,
        int* sigFigs)
{
    FILE* file = fopen(information->fileName, "r");
    char line[LINE_BUFFER];
    while (fgets(line, sizeof(line), file) != NULL) {
        int numberEquals = 0;
        int result = download_setup(line, &numberEquals);
        if (result != 0) {
            continue;
        }
        result = detect_range_print(line, variables, loops, sigFigs);
        if (result != 0) {
            continue;
        }
        result = detect_loops(line, variables, loops, sigFigs);
        if (result != 0) {
            continue;
        }
        if (numberEquals == 1) {
            char* variableName = strtok(line, "=");
            char* expression = strtok(NULL, "=");
            int start = 0;
            int end = strlen(variableName) - 1;
            while (isspace(variableName[start]) && start < end) {
                start++;
            }
            while (isspace(variableName[end]) && end > start) {
                end--;
            }
            memmove(variableName, variableName + start, end - start + 1);
            variableName[end - start + 1] = '\0';
            int result = download_assignment_check_valid(variableName);
            if (result != 0) {
                continue;
            }
            result = download_assignment(
                    loops, variables, expression, variableName, sigFigs);
            if (result != 0) {
                continue;
            }
        } else if (numberEquals == 0) {
            download_expression(variables, loops, line, sigFigs);
        }
        memset(line, 0, sizeof(line));
    }
    fclose(file);
    return 0;
}

/* Frees dynamically allocated memory for all structures and arrays
 *
 * int* sigFigs: pointer to nunber of sig figs to print doubles to
 * Information* information: a structure containing file names, variable styring
 * and loop strings from the command line int* numberVariables: a pointer to
 * number of variables from command line int* numberLoops: a pointer to number
 * of loops from command line Variables* variables: A poointer to variables
 * struct which contains variables Loops* loops: A pointer to loops struct which
 * contains loops
 *
 * Returns 0
 */
int free_memory(int* sigFigs, Information* information, int* numberVariables,
        int* numberLoops, Variables* variables, Loops* loops)
{
    free((void*)sigFigs);
    free((void*)information->fileName);
    free((void*)information->variableStrings);
    free((void*)information->loopsStrings);
    free((void*)information);
    free((void*)numberVariables);
    free((void*)numberLoops);
    for (int i = 0; i < variables->size; i++) {
        if (variables->names[i]) {
            free((void*)variables->names[i]);
        }
    }
    free((void*)variables->names);
    free((void*)variables->values);
    free((void*)variables);
    for (int i = 0; i < loops->size; i++) {
        if (loops->names[i]) {
            free((void*)loops->names[i]);
        }
    }
    free((void*)loops->names);
    free((void*)loops->startingValue);
    free((void*)loops->currentValue);
    free((void*)loops->increment);
    free((void*)loops->endValue);
    free((void*)loops);
    return 0;
}

/* Handles initial command-line processing by parsing command line arguments,
 * checking file validity, and decoding varaible and loops
 *
 * int argc: number of command-line arguments
 * char* argv[]: An array of the command line strings
 * int* sigFigs: pointer to number of sig figs to print doubles to
 * Information* information: pointer to struct containing variable and loop
 * strings from command line and file name int* numberVariables: A pointer to
 * integer tracking number of variables on initial command line int*
 * numberLoops: Pointer to int tracking numbver of loops on initial command line
 * Variables* variabvles: Popinter to variables struct which contains varaibles
 * Loops* loops: Pointer to loops struct which contains loops
 *
 * Return 0 if success, INVALID_COMMAND_LINE_ERROR if commandline is invalid
 * format, FILE_DOES_NOT_EXOST if inoput file is provided but cannot be
 * openeing, INVALID_VARIABLES_ERROR if invalid variables are encountered and
 * DUPLICATE_VARIABLES_ERROR if duplicate variable names are used on command
 * line
 */
int run_initial_command_line(int argc, char* argv[], int* sigFigs,
        Information* information, int* numberVariables, int* numberLoops,
        Variables* variables, Loops* loops)
{
    int result = download_command_line(
            argc, argv, sigFigs, information, numberVariables, numberLoops);
    if (result == INVALID_COMMAND_LINE_ERROR) {
        free_memory(sigFigs, information, numberVariables, numberLoops,
                variables, loops);
        fprintf(stderr,
                "Usage: ./uqexpr [--loopable string] [--define string] "
                "[--significantfigures 2..8] [inputfilename]\n");
        return INVALID_COMMAND_LINE_ERROR;
    }
    if (information->fileName != NULL && strcmp(information->fileName, "")) {
        result = check_open_file(information);
        if (result == FILE_DOES_NOT_OPEN_ERROR) {
            fprintf(stderr, "uqexpr: can't open file \"%s\" for reading\n",
                    information->fileName);
            free_memory(sigFigs, information, numberVariables, numberLoops,
                    variables, loops);
            return FILE_DOES_NOT_OPEN_ERROR;
        }
    }
    result = decode_variable_strings(variables, information, numberVariables);
    int resultTwo
            = decode_loops_strings(loops, information, numberLoops, variables);
    if (result == INVALID_VARIABLES_ERROR
            || resultTwo == INVALID_VARIABLES_ERROR) {
        free_memory(sigFigs, information, numberVariables, numberLoops,
                variables, loops);
        fprintf(stderr, "uqexpr: invalid variable(s) were found\n");
        return INVALID_VARIABLES_ERROR;
    }
    if (result == DUPLICATE_VARIABLES_ERROR
            || resultTwo == DUPLICATE_VARIABLES_ERROR) {
        free_memory(sigFigs, information, numberVariables, numberLoops,
                variables, loops);
        fprintf(stderr, "uqexpr: one or more variables are duplicated\n");
        return DUPLICATE_VARIABLES_ERROR;
    }
    return 0;
}

/* Manages reading a file or user input in live command line, for both computes
 * expressions, completes assignments and runs special functions like @range,
 * @loops, @print
 *
 * Variables* variables: A pointer to variables struct which contains variables
 * int* sigFigs: A pointer to integer which determines number of sig figs to
 * print doubles to Loops* loops: A pointer to loops struct which contains loops
 * Information* information: Pointer to Information struct which contains file
 * name, and variable and loop strings from command line int* numberVariables:
 * pointer to number of variables detected on command line initially int*
 * numberLoops: pointer to number of loops detected on command line initially
 *
 * Return 0
 */
int run_program(Variables* variables, int* sigFigs, Loops* loops,
        Information* information, int* numberVariables, int* numberLoops)
{
    printf("Welcome to uqexpr!\nWritten by s4809233.\n");
    if (variables->size == 0) {
        printf("No variables were defined.\n");
    } else {
        printf("Variables:\n");
        char format[FORMAT_BUFFER_SIZE];
        snprintf(format, sizeof(format), "%%.%dg", sigFigs[0]);
        for (int i = 0; i < variables->size; i++) {
            printf("%s = ", variables->names[i]);
            printf(format, variables->values[i]);
            printf("\n");
        }
    }
    if (loops->size == 0) {
        printf("No loop variables were defined.\n");
    } else {
        printf("Loop variables:\n");
        char format[FORMAT_BUFFER_SIZE];
        snprintf(format, sizeof(format), "%%.%dg", sigFigs[0]);
        for (int i = 0; i < loops->size; i++) {
            printf("%s = ", loops->names[i]);
            printf(format, loops->currentValue[i]);
            printf(" (");
            printf(format, loops->startingValue[i]);
            printf(", ");
            printf(format, loops->increment[i]);
            printf(", ");
            printf(format, loops->endValue[i]);
            printf(")\n");
        }
    }
    if (strcmp(information->fileName, "")) {
        download_file(information, variables, loops, sigFigs);
    } else {
        printf("Please enter your expressions and assignment "
               "operations.\n");
        int tracker = 0;
        while (tracker == 0) {
            tracker = download_live_command_line(variables, loops, sigFigs);
        }
    }
    printf("Thank you for using uqexpr.\n");
    free_memory(sigFigs, information, numberVariables, numberLoops, variables,
            loops);
    return 0;
}

/* Initialises memory, processes command line arguments and calls functions
 * responsible for executing the program
 *
 * int argc: number of command line arguments
 * char* argv[]: array of command line strings
 *
 * Return with error code from run_initial_command or if successful returns 0
 */
int main(int argc, char* argv[])
{
    int* sigFigs = (int*)malloc(sizeof(int));
    int* numberVariables = (int*)malloc(sizeof(int));
    int* numberLoops = (int*)malloc(sizeof(int));
    Variables* variables = (Variables*)malloc(sizeof(Variables));
    Loops* loops = (Loops*)malloc(sizeof(Loops));
    Information* information = (Information*)malloc(sizeof(Information));
    information->fileName = (char*)malloc(sizeof(char));
    information->variableStrings = (char**)malloc(sizeof(char*));
    information->loopsStrings = (char**)malloc(sizeof(char*));
    int result = run_initial_command_line(argc, argv, sigFigs, information,
            numberVariables, numberLoops, variables, loops);
    if (result != 0) {
        return result;
    }
    result = run_program(variables, sigFigs, loops, information,
            numberVariables, numberLoops);
    if (result != 0) {
        return result;
    }
    return 0;
}
