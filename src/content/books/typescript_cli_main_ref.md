---
description: Implementing core linux command line applications in Typescript
imgSrc: https://friendlyuser.github.io/imgs/2023/2257408743.png
pubDate: Fri, 14 December 2024
tags:
- go
title: Introduction to Typescript and Command Line Applications
---

# Chapter 1: Introduction to Typescript and Command Line Applications

Why Typescript is a good choice for building command line applications?
TypeScript is a statically-typed superset of JavaScript that provides optional static typing, classes, and interfaces. This makes it easier to write and maintain large-scale applications, including command-line applications.

By using TypeScript, developers can catch errors early in the development process and enjoy the benefits of code completion, which helps save time and reduces the likelihood of errors. Additionally, TypeScript can be transpiled to JavaScript, allowing developers to use modern JavaScript features while still supporting older browsers and environments.

In the context of command-line applications, TypeScript can also provide better documentation and easier maintenance due to its explicit type annotations and clear interfaces. This can help prevent common errors that might otherwise occur when working with complex command-line applications.

Basic concepts and tools for building command line applications in Typescript
To build command-line applications in TypeScript, there are a few basic concepts and tools that developers should be familiar with:

1. Command-line interfaces (CLIs): CLIs are programs that allow users to interact with a software application through a command-line interface, using text-based commands and responses.
2. Node.js: Node.js is a runtime environment that allows developers to run JavaScript code outside of a web browser. It provides built-in modules for working with files, networks, and other system resources, making it an ideal platform for building command-line applications.
3. Command-line argument parsing: Command-line argument parsing is the process of extracting input arguments and options from a command-line interface. There are several libraries available in TypeScript, such as Commander.js and Yargs, that simplify this process.
4. Standard Input/Output (stdin/stdout): Standard input (stdin) and standard output (stdout) are the default streams used by command-line applications for input and output, respectively. These streams can be accessed in TypeScript using the `process.stdin` and `process.stdout` objects.
5. Console logging: Console logging is a common way to provide output from a command-line application. In TypeScript, console logging can be done using the `console.log()` function.

By mastering these concepts and tools, developers can build powerful and user-friendly command-line applications in TypeScript.

# Chapter 2: Building a Tail Command

Overview of the tail command and its functionality
The `tail` command is a popular Unix/Linux command-line utility that is used to display the last few lines of a text file. It can be used to monitor log files or other constantly updating files in real-time. The `tail` command is also useful for quickly reviewing the end of a file without having to load the entire file into memory.

The basic syntax for the `tail` command is as follows:

```typescript
tail [options] [file]
```

The `file` argument specifies the file to be displayed. If no file is specified, `tail` will read from standard input.

Some of the most commonly used options for the `tail` command include:

* `-n` or `--lines`: Specifies the number of lines to display. For example, `tail -n 10 file.txt` will display the last 10 lines of the file.
* `-f` or `--follow`: Causes `tail` to continuously monitor the file for changes and display any new lines that are added. This is useful for monitoring log files or other constantly updating files in real-time.
* `-q` or `--quiet`: Suppresses the display of file names when multiple files are specified.
* `-v` or `--verbose`: Displays file names when multiple files are specified, even if only one file is being displayed.
* `-c` or `--bytes`: Specifies the number of bytes to display rather than the number of lines.

The `tail` command is a powerful and versatile tool that can be used to quickly and easily view the end of a text file, monitor log files in real-time, and more.

Designing the Typescript implementation of the tail command
To implement the `tail` command in TypeScript, we can follow these steps:

1. Parse the command-line arguments to determine which file(s) to read and how many lines to display.
2. Open the specified file(s) for reading.
3. Seek to the end of the file(s) to read the last n lines.
4. Read the last n lines of the file(s) and output them to the console.

Here's an example implementation of the `tail` command in TypeScript:

```typescript
import * as fs from 'fs';

function tail(file: string, numLines: number) {
 // Open the file for reading
 const fd = fs.openSync(file, 'r');
 let fileData = '';
 let bytesRead;
 const bufSize = 1024;

 // Read the file in chunks until we have enough lines
 while (fileData.split('\n').length < numLines) {
  const buf = Buffer.alloc(bufSize);
  bytesRead = fs.readSync(fd, buf, 0, bufSize, null);
  if (!bytesRead) break;
  fileData = buf.toString() + fileData;
 }

 // Split the file data into lines and output the last n lines
 const lines = fileData.split('\n');
 const startLine = Math.max(lines.length - numLines, 0);
 console.log(lines.slice(startLine).join('\n'));
 
 // Close the file
 fs.closeSync(fd);
}

// Parse the command-line arguments
const args = process.argv.slice(2);
const numLines = args.length > 0 ? parseInt(args[0]) : 10;
const files = args.slice(1);

// Call the tail function for each file
for (const file of files) {
 tail(file, numLines);
}
```

In this implementation, we use the `fs` module from Node.js to open and read the file(s) specified on the command line. We read the file in chunks to avoid reading the entire file into memory at once. We then split the file data into lines and output the last n lines to the console.

This implementation supports reading from multiple files and allows the user to specify the number of lines to display as a command-line argument (defaulting to 10 if no argument is provided).

Reading and processing the input file
To read and process the input file for the `tail` command, we need to open the file and read its contents. Here's an updated version of our `tail` function that reads the input file and splits its contents into lines:

```typescript
import * as fs from 'fs';

function tail(file: string, numLines: number) {
 // Open the file for reading
 const fd = fs.openSync(file, 'r');
 let fileData = '';
 let bytesRead;
 const bufSize = 1024;

 // Read the file in chunks until we have enough lines
 while (fileData.split('\n').length < numLines) {
  const buf = Buffer.alloc(bufSize);
  bytesRead = fs.readSync(fd, buf, 0, bufSize, null);
  if (!bytesRead) break;
  fileData = buf.toString() + fileData;
 }

 // Split the file data into lines
 const lines = fileData.split('\n');

 // Process each line of the file
 for (const line of lines) {
 // TODO: Process the line here
 }

 // Close the file
 fs.closeSync(fd);
}
```

In this updated version, we still open the file for reading and read its contents in chunks until we have enough lines. We then split the file data into lines using the `split` method.

To process each line of the file, we can use a `for...of` loop to iterate over the `lines` array. We can then process each line as needed, depending on the requirements of our application. For example, we might want to filter out certain lines, search for specific patterns, or perform calculations on numeric values.

Once we have processed each line of the file, we can close the file using the `fs.closeSync` method to free up system resources.

Note that this implementation assumes that the input file contains text data. If the file contains binary data, we would need to use a different method of reading and processing the file. Additionally, we should handle errors that may occur when opening or reading the file, such as if the file does not exist or if we do not have permission to read it.

Implementing the tail functionality
To implement the `tail` functionality of displaying the last n lines of the input file, we can modify our existing `tail` function to output the last n lines to the console:

```typescript
import * as fs from 'fs';

function tail(file: string, numLines: number) {
 // Open the file for reading
 const fd = fs.openSync(file, 'r');
 let fileData = '';
 let bytesRead;
 const bufSize = 1024;

 // Read the file in chunks until we have enough lines
 while (fileData.split('\n').length < numLines) {
  const buf = Buffer.alloc(bufSize);
  bytesRead = fs.readSync(fd, buf, 0, bufSize, null);
  if (!bytesRead) break;
  fileData = buf.toString() + fileData;
 }

 // Split the file data into lines and output the last n lines
 const lines = fileData.split('\n');
 const startLine = Math.max(lines.length - numLines, 0);
 console.log(lines.slice(startLine).join('\n'));
 
 // Close the file
 fs.closeSync(fd);
}
```

In this implementation, we split the file data into lines as before, but we now use the `slice` method to extract the last `numLines` lines of the file. We then use the `join` method to join the lines into a single string with newline characters between each line, and we output this string to the console using the `console.log` method.

Note that this implementation assumes that the input file contains text data and that each line is terminated by a newline character. If the file contains binary data or if the lines are not terminated by newlines, we would need to use a different method of reading and processing the file. Additionally, we should handle errors that may occur when opening or reading the file, such as if the file does not exist or if we do not have permission to read it.

Outputting the results to the console
In our implementation of the `tail` function, we output the last `numLines` lines of the input file to the console using the `console.log` method. We can now wrap our `tail` function in a `main` function that accepts command line arguments and calls the `tail` function with the appropriate arguments:

```typescript
import * as fs from 'fs';

function tail(file: string, numLines: number) {
 // Open the file for reading
 const fd = fs.openSync(file, 'r');
 let fileData = '';
 let bytesRead;
 const bufSize = 1024;

 // Read the file in chunks until we have enough lines
 while (fileData.split('\n').length < numLines) {
 const buf = Buffer.alloc(bufSize);
 bytesRead = fs.readSync(fd, buf, 0, bufSize, null);
 if (!bytesRead) break;
 fileData = buf.toString() + fileData;
 }

 // Split the file data into lines and output the last n lines
 const lines = fileData.split('\n');
 const startLine = Math.max(lines.length - numLines, 0);
 console.log(lines.slice(startLine).join('\n'));

 // Close the file
 fs.closeSync(fd);
}

function main() {
 // Get the command line arguments
 const args = process.argv.slice(2);
 
 // Check that we have the correct number of arguments
 if (args.length !== 2) {
  console.error('Usage: tail <file> <numLines>');
  process.exit(1);
 }

 // Parse the command line arguments
 const file = args[0];
 const numLines = parseInt(args[1], 10);

 // Call the tail function with the parsed arguments
 tail(file, numLines);
}

main();
```

In this implementation, we define a `main` function that first checks that we have the correct number of command line arguments (two: the input file and the number of lines to output). If we do not have the correct number of arguments, we output an error message to the console and exit the program using the `process.exit` method.

If we have the correct number of arguments, we parse them using the `parseInt` method and call the `tail` function with the parsed arguments.

When we run this program from the command line with the appropriate arguments (e.g., `node tail.js input.txt 10`), it will output the last 10 lines of the `input.txt` file to the console.

Testing the tail command
To test the `tail` command, we can create a test file with some sample text data and then run the `tail` command with different numbers of lines to ensure that it outputs the correct results.

For example, we can create a test file called `test.txt` with the following contents:

```typescript
Line 1
Line 2
Line 3
Line 4
Line 5
Line 6
Line 7
Line 8
Line 9
Line 10
```

We can then run the `tail` command with different numbers of lines and verify that it outputs the correct results:

```bash
$ node tail.js test.txt 3
Line 8
Line 9
Line 10

$ node tail.js test.txt 5
Line 6
Line 7
Line 8
Line 9
Line 10

$ node tail.js test.txt 10
Line 1
Line 2
Line 3
Line 4
Line 5
Line 6
Line 7
Line 8
Line 9
Line 10
```

We should also test the `tail` command with invalid arguments (e.g., a non-existent input file or a non-numeric number of lines) to ensure that it handles errors correctly:

```bash
$ node tail.js invalid.txt 5
Error: ENOENT: no such file or directory, open 'invalid.txt'

$ node tail.js test.txt foo
Error: parseInt failed on "foo"
```

By testing the `tail` command with various input files and numbers of lines, we can ensure that it works correctly and handles errors gracefully.

# Chapter 3: Building a Head Command

Overview of the head command and its functionality
The `head` command is a common Unix command that outputs the first few lines of a file. By default, it outputs the first 10 lines of the file, but this can be changed with the `-n` option followed by a number to specify the number of lines to output.

The basic syntax of the `head` command is:

```typescript
head [options] [file]
```

where `options` is an optional set of command line options (such as `-n`) and `file` is the input file to read.

The most commonly used option for the `head` command is `-n`, which specifies the number of lines to output. For example, to output the first 20 lines of a file, we can use the following command:

```typescript
head -n 20 input.txt
```

If no option is specified, `head` will output the first 10 lines of the input file:

```typescript
head input.txt
```

The `head` command is often used in combination with other commands in Unix pipelines to extract the first few lines of a file or to quickly check the contents of a file.

Designing the Typescript implementation of the head command
To implement the `head` command in Typescript, we can follow a similar approach to the `tail` command. We'll need to read the input file and output the specified number of lines to the console. Here's a possible design for the Typescript implementation of the `head` command:

1. Parse the command line arguments to determine the input file and the number of lines to output.
2. Read the input file and split it into lines.
3. Output the specified number of lines to the console.
4. Handle errors gracefully, such as when the input file doesn't exist or the number of lines is invalid.

Here's an example of how this might look in Typescript:

```typescript
import { readFileSync } from 'fs';

function head(file: string, numLines: number) {
 try {
 // Read the input file and split it into lines
 const lines = readFileSync(file, 'utf-8').split('\n');

 // Output the specified number of lines to the console
 for (let i = 0; i < numLines; i++) {
 console.log(lines[i]);
 }
 } catch (error) {
 console.error(`Error: ${error.message}`);
 }
}

// Parse the command line arguments
const file = process.argv[2];
const numLines = parseInt(process.argv[3]);

// Call the head function with the specified arguments
head(file, numLines);
```

In this example, we use the built-in `readFileSync` function from the Node.js `fs` module to read the input file and split it into lines. We then use a simple for loop to output the specified number of lines to the console.

We also handle errors gracefully using a try-catch block to catch any errors that might occur when reading the file or outputting the lines. Finally, we parse the command line arguments using `process.argv` and call the `head` function with the specified arguments.

This is just one possible implementation of the `head` command in Typescript, but it should give you a good idea of the basic approach.

Reading and processing the input file
To read and process the input file for the `head` command in Typescript, we can use the built-in `readFileSync` function from the Node.js `fs` module. This function reads the contents of a file synchronously and returns the contents as a string.

Once we have the contents of the file, we can split it into lines using the `split` function, which splits a string into an array of substrings based on a specified delimiter. In the case of the `head` command, the delimiter is the newline character `\n`, which separates the lines of the file.

Here's an updated version of the `head` function that reads and processes the input file:

```typescript
import { readFileSync } from 'fs';

function head(file: string, numLines: number) {
 try {
 // Read the input file and split it into lines
 const contents = readFileSync(file, 'utf-8');
 const lines = contents.split('\n');

 // Output the specified number of lines to the console
 for (let i = 0; i < numLines; i++) {
 console.log(lines[i]);
 }
 } catch (error) {
 console.error(`Error: ${error.message}`);
 }
}
```

In this updated version, we first call `readFileSync` with the filename and the encoding `'utf-8'` to read the contents of the file as a string. We then split the contents into an array of lines using the `split` function and the delimiter `\n`. We can now iterate over the first `numLines` elements of the `lines` array and output them to the console using `console.log`.

Implementing the head functionality
To implement the `head` functionality in Typescript, we can start with the basic structure of the `head` function and add code to handle the following cases:

1. When the input file is empty or contains fewer lines than the requested number of lines.
2. When the requested number of lines is greater than the total number of lines in the input file.

Here's an example implementation of the `head` function that handles these cases:

```typescript
import { readFileSync } from 'fs';

function head(file: string, numLines: number) {
 try {
 // Read the input file and split it into lines
 const contents = readFileSync(file, 'utf-8');
 const lines = contents.split('\n');

 // Handle empty or short files
 if (lines.length === 1 && lines[0] === '') {
 console.log('');
 return;
 } else if (numLines > lines.length) {
 numLines = lines.length;
 }

 // Output the specified number of lines to the console
 for (let i = 0; i < numLines; i++) {
 console.log(lines[i]);
 }
 } catch (error) {
 console.error(`Error: ${error.message}`);
 }
}
```

In this implementation, we first check if the input file is empty or contains only one line with an empty string. In this case, we output an empty string and return early from the function. We then check if the requested number of lines is greater than the total number of lines in the input file. If so, we set `numLines` to the total number of lines.

Finally, we iterate over the first `numLines` elements of the `lines` array and output them to the console using `console.log`.

Outputting the results to the console
To output the results of the `head` command to the console, we can simply call the `head` function with the appropriate arguments and let the function handle the output. Here's an example of how we can call the `head` function to output the first 10 lines of a file called `example.txt`:

```typescript
head('example.txt', 10);
```

This will read the contents of `example.txt`, split them into lines, and output the first 10 lines to the console. If the file contains fewer than 10 lines, it will output all the lines in the file.

We can also modify the `head` function to return the first `numLines` lines of the input file as a string instead of outputting them to the console. Here's an example implementation:

```typescript
import { readFileSync } from 'fs';

function head(file: string, numLines: number): string {
 try {
 // Read the input file and split it into lines
 const contents = readFileSync(file, 'utf-8');
 const lines = contents.split('\n');

 // Handle empty or short files
 if (lines.length === 1 && lines[0] === '') {
 return '';
 } else if (numLines > lines.length) {
 numLines = lines.length;
 }

 // Return the specified number of lines as a string
 return lines.slice(0, numLines).join('\n');
 } catch (error) {
 console.error(`Error: ${error.message}`);
 return '';
 }
}
```

In this implementation, we first check if the input file is empty or contains only one line with an empty string. In this case, we return an empty string. We then check if the requested number of lines is greater than the total number of lines in the input file. If so, we set `numLines` to the total number of lines.

Finally, we use the `slice` function to extract the first `numLines` elements of the `lines` array, and then join them into a single string with the newline character `\n`. We then return this string from the function.

Testing the head command
To test the `head` command, we can create a test file with some example contents and then call the `head` function with different arguments to verify that it produces the expected output.

Here's an example of how we can test the `head` function using the Jest testing framework:

```typescript
import { head } from './head';

describe('head', () => {
 it('should return an empty string for an empty file', () => {
 expect(head('test/empty.txt', 10)).toBe('');
 });

 it('should return all lines for a file with fewer lines than requested', () => {
 expect(head('test/short.txt', 10)).toBe('Line 1\nLine 2\nLine 3');
 });

 it('should return the first n lines of a file', () => {
 expect(head('test/long.txt', 3)).toBe('Line 1\nLine 2\nLine 3');
 });

 it('should return all lines for a file with fewer lines than requested', () => {
 expect(head('test/long.txt', 10)).toBe('Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6\nLine 7\nLine 8\nLine 9\nLine 10');
 });
});
```

In this example, we have four test cases:

1. An empty file, which should return an empty string.
2. A file with fewer lines than requested, which should return all the lines in the file.
3. A file with more lines than requested, which should return the first `n` lines of the file.
4. A file with more lines than requested, but fewer lines than the total number of lines in the file. This test case is similar to the previous one, but with a larger value of `n`.

For each test case, we call the `head` function with the appropriate arguments and use the `expect` function to verify that the function returns the expected result.

# Chapter 4: Building a Du Command

Overview of the du command and its functionality
The `du` command is a Unix-based command-line utility that is used to estimate the space usage of a file or directory. The name "du" stands for "disk usage".

The `du` command displays the number of blocks used by a file or directory on the file system. By default, the `du` command displays the space used by the entire directory and all of its subdirectories. It can also display the size of individual files within the directory.

The `du` command has several options that allow you to customize the output. For example, you can use the `-h` option to display the sizes in human-readable format (such as "1K", "2M", etc.), or you can use the `-s` option to display only the total size of the directory without showing the sizes of its subdirectories.

Designing the Typescript implementation of the du command
To implement the `du` command in Typescript, we will need to recursively traverse the directory hierarchy and calculate the size of each file and subdirectory. We can then sum up the sizes to obtain the total size of the directory. Here is an outline of the steps we will need to follow:

1. Parse the command-line arguments to determine the directory to be analyzed and any options that were specified.
2. Use the Node.js `fs` module to check if the specified directory exists and is accessible.
3. Traverse the directory hierarchy using the `fs.readdirSync` function to obtain a list of all files and subdirectories within the directory.
4. For each file, use the `fs.statSync` function to obtain the file size and add it to the total size of the directory.
5. For each subdirectory, recursively call the `du` function to obtain its size and add it to the total size of the directory.
6. Output the total size of the directory in the appropriate format, based on any options that were specified.

Here's an example of what the code might look like:

```typescript
import * as fs from 'fs';

function du(directory: string, options?: { humanReadable: boolean }): number {
 let totalSize = 0;

 const files = fs.readdirSync(directory);

 for (const file of files) {
 const path = `${directory}/${file}`;
 const stats = fs.statSync(path);

 if (stats.isFile()) {
 totalSize += stats.size;
 } else if (stats.isDirectory()) {
 totalSize += du(path);
 }
 }

 if (options?.humanReadable) {
 return formatSize(totalSize);
 } else {
 return totalSize;
 }
}

function formatSize(sizeInBytes: number): string {
 // Implementation of human-readable formatting
}

const directory = process.argv[2];
const options = process.argv.slice(3).includes('-h') ? { humanReadable: true } : undefined;

console.log(du(directory, options));
```

In this implementation, the `du` function takes a directory path as its first argument and an optional options object as its second argument. The `options` object can have a `humanReadable` property, which is a boolean indicating whether the output should be in human-readable format.

The `du` function recursively traverses the directory hierarchy and calculates the total size of the directory. For each file, it uses the `fs.statSync` function to obtain the file size and adds it to the total size of the directory. For each subdirectory, it recursively calls the `du` function to obtain its size and adds it to the total size of the directory.

The `formatSize` function takes a size in bytes and returns a human-readable string representation of the size, such as "1K", "2M", etc.

Finally, the script reads the command-line arguments and calls the `du` function with the appropriate arguments. The result is output to the console. If the `-h` option was specified, the size is displayed in human-readable format.

If the `-h` option is specified, the `du` function returns the result in human-readable format by calling the `formatSize` function. The `formatSize` function takes the total size in bytes as its argument and returns a human-readable string representation of the size. Here's an example implementation of the `formatSize` function:

```typescript
function formatSize(sizeInBytes: number): string {
 const units = ['B', 'K', 'M', 'G', 'T', 'P'];
 let size = sizeInBytes;
 let unitIndex = 0;
 
 while (size >= 1024 && unitIndex < units.length - 1) {
 size /= 1024;
 unitIndex++;
 }

 return `${size.toFixed(1)}${units[unitIndex]}`;
}
```

This implementation uses an array of unit names, such as "B" for bytes, "K" for kilobytes, "M" for megabytes, etc. It iteratively divides the total size by 1024 until the result is less than 1024 or the largest unit has been reached. It then formats the result as a string with one decimal place and the appropriate unit name.

If the `-h` option was not specified, the `du` function returns the result in bytes as a number. The caller is responsible for formatting the result as appropriate.

With this implementation, we can now calculate the total size of a directory using the `du` command in Typescript. We can run the script with a command like this:

```typescript
node du.ts /path/to/directory
```

If we want the output to be in human-readable format, we can use the `-h` option:

```typescript
node du.ts /path/to/directory -h
```

Reading and processing the input directory
To calculate the total size of a directory in Typescript, we first need to read and process the input directory. We can use the `fs` module in Node.js to do this.

Here's an example implementation of the `readDirectory` function, which reads the contents of a directory and returns an array of file and directory names:

```typescript
import * as fs from 'fs';

async function readDirectory(directoryPath: string): Promise<string[]> {
 return new Promise((resolve, reject) => {
 fs.readdir(directoryPath, (err, files) => {
 if (err) {
 reject(err);
 } else {
 resolve(files);
 }
 });
 });
}
```

This implementation uses the `fs.readdir` function to read the contents of the directory. It returns a promise that resolves with an array of file and directory names. If an error occurs, the promise is rejected with the error.

Next, we can use the `readDirectory` function to process the input directory and calculate its size. We can use the `fs.stat` function to get information about each file and directory in the directory. We can then add up the sizes of all the files and recursively process any subdirectories.

Here's an example implementation of the `du` function that calculates the total size of a directory:

```typescript
import * as fs from 'fs';

async function du(directoryPath: string, humanReadable: boolean = false): Promise<number|string> {
 let totalSize = 0;
 const files = await readDirectory(directoryPath);

 for (const file of files) {
 const filePath = `${directoryPath}/${file}`;
 const stats = await stat(filePath);

 if (stats.isDirectory()) {
 const subdirectorySize = await du(filePath, humanReadable);
 totalSize += subdirectorySize;
 } else {
 totalSize += stats.size;
 }
 }

 return humanReadable ? formatSize(totalSize) : totalSize;
}
```

This implementation first initializes the `totalSize` variable to 0. It then reads the contents of the directory using the `readDirectory` function and loops over each file and directory. For each file, it uses the `fs.stat` function to get information about the file. If the file is a directory, it recursively calls the `du` function to get the size of the subdirectory. Otherwise, it adds the size of the file to the `totalSize` variable.

At the end of the function, it returns the `totalSize` variable, or the result of calling the `formatSize` function on the `totalSize` variable if the `humanReadable` option is `true`.

Implementing the du functionality
To implement the `du` functionality in Typescript, we can use the `du` function we designed earlier, along with some additional code to handle the command-line arguments and print the output to the console.

Here's an example implementation of the `du` command:

```typescript
import * as fs from 'fs';

async function readDirectory(directoryPath: string): Promise<string[]> {
 return new Promise((resolve, reject) => {
 fs.readdir(directoryPath, (err, files) => {
 if (err) {
 reject(err);
 } else {
 resolve(files);
 }
 });
 });
}

async function stat(filePath: string): Promise<fs.Stats> {
 return new Promise((resolve, reject) => {
 fs.stat(filePath, (err, stats) => {
 if (err) {
 reject(err);
 } else {
 resolve(stats);
 }
 });
 });
}

function formatSize(size: number): string {
 const units = ['B', 'KB', 'MB', 'GB', 'TB'];
 let unitIndex = 0;
 let formattedSize = size;

 while (formattedSize >= 1024 && unitIndex < units.length - 1) {
 formattedSize /= 1024;
 unitIndex++;
 }

 return `${formattedSize.toFixed(2)} ${units[unitIndex]}`;
}

async function du(directoryPath: string, humanReadable: boolean = false): Promise<number|string> {
 let totalSize = 0;
 const files = await readDirectory(directoryPath);

 for (const file of files) {
 const filePath = `${directoryPath}/${file}`;
 const stats = await stat(filePath);

 if (stats.isDirectory()) {
 const subdirectorySize = await du(filePath, humanReadable);
 totalSize += subdirectorySize;
 } else {
 totalSize += stats.size;
 }
 }

 return humanReadable ? formatSize(totalSize) : totalSize;
}

async function main() {
 const args = process.argv.slice(2);
 const directoryPath = args[0];
 const humanReadable = args.includes('-h');

 try {
 const totalSize = await du(directoryPath, humanReadable);
 console.log(humanReadable ? `${formatSize(totalSize as number)} total` : totalSize);
 } catch (error) {
 console.error(error.message);
 process.exit(1);
 }
}

main();
```

This implementation first defines the `readDirectory`, `stat`, `formatSize`, and `du` functions we designed earlier. It then defines a `main` function that parses the command-line arguments using `process.argv`, calls the `du` function with the appropriate arguments, and prints the output to the console using `console.log`.

To run the `du` command, you can save the above code to a file (e.g. `du.ts`) and then compile it to JavaScript using the Typescript compiler (`tsc du.ts`). You can then run the resulting JavaScript file using Node.js (`node du.js`), passing the directory you want to calculate the size of as the first argument and the `-h` option if you want the size to be displayed in human-readable format. For example:

```typescript
$ node du.js /path/to/directory -h
4.54 MB total
```

Outputting the results to the console
To output the results to the console, we can use the `console.log()` method to print the size of each directory. If the `-h` option is specified, we can use the `filesize` library to convert the size into human-readable format.

Here's the implementation:

```typescript
import fs from 'fs';
import path from 'path';
import filesize from 'filesize';

export function duCommand(directoryPath: string, humanReadable: boolean): void {
 const directorySize = getDirectorySize(directoryPath);
 const formattedSize = humanReadable ? filesize(directorySize) : directorySize;

 console.log(`${directoryPath}\t${formattedSize}`);
}

function getDirectorySize(directoryPath: string): number {
 let size = 0;

 const files = fs.readdirSync(directoryPath);

 files.forEach(file => {
 const filePath = path.join(directoryPath, file);
 const stat = fs.statSync(filePath);

 if (stat.isFile()) {
 size += stat.size;
 } else {
 size += getDirectorySize(filePath);
 }
 });

 return size;
}
```

In the `duCommand` function, we first call the `getDirectorySize` function to get the total size of the directory. We then format the size based on whether the `-h` option was specified or not. Finally, we use `console.log()` to output the results to the console.

The `getDirectorySize` function recursively iterates over all the files and subdirectories in the given directory and calculates their size using the `fs.statSync()` method. If the file is a regular file, its size is added to the total size. If the file is a directory, the function is called recursively on the directory to get its size.

Note that the `du` command only accepts a single directory as input, so we don't need to handle multiple directories like we did in the `tail` and `head` commands.

Testing the du command
To test the `du` command, we can create a test directory with some files and subdirectories, and then run the command on that directory. We can then verify that the output is correct based on the expected sizes of the files and directories.

Here's an example implementation of a test case:

```typescript
import { duCommand } from './du';

describe('duCommand', () => {
 it('should output the correct size of the directory', () => {
 const testDir = './testDir';
 const file1 = 'file1.txt';
 const file2 = 'file2.txt';
 const subDir = 'subdir';
 const subDirFile = 'subdirfile.txt';
 const subSubDir = 'subsubdir';
 const subSubDirFile = 'subsubdirfile.txt';

 // create test directory with files and subdirectories
 fs.mkdirSync(testDir);
 fs.writeFileSync(path.join(testDir, file1), '1234567890');
 fs.writeFileSync(path.join(testDir, file2), 'abcdefghijklmnopqrstuvwxyz');
 fs.mkdirSync(path.join(testDir, subDir));
 fs.writeFileSync(path.join(testDir, subDir, subDirFile), '1');
 fs.mkdirSync(path.join(testDir, subDir, subSubDir));
 fs.writeFileSync(path.join(testDir, subDir, subSubDir, subSubDirFile), '12');

 // run du command on test directory
 const expectedSize = 38;
 const formattedSize = '38 B';
 const output = captureConsole(() => duCommand(testDir, true));

 // verify output
 expect(output).toEqual(`${testDir}\t${formattedSize}\n`);
 });
});

function captureConsole(fn: () => void): string {
 const oldConsole = global.console;
 const output = new Array<string>();
 global.console = { ...oldConsole, log: (msg: string) => output.push(msg) };
 fn();
 global.console = oldConsole;
 return output.join('');
}
```

In this test case, we create a test directory with some files and subdirectories using the `fs` module. We then call the `duCommand` function on the test directory with the `-h` option set to `true` to get the human-readable size.

We then capture the output of the `console.log()` method using the `captureConsole` helper function, and verify that the output is correct based on the expected size of the directory.

Note that we use the `captureConsole` function to capture the output of the `console.log()` method, since we don't want to actually output anything to the console during testing.

# Chapter 5: Building a Df Command

Overview of the df command and its functionality
The `df` command is used to display the amount of free and used disk space on a file system. It can be used to get an overview of the available disk space on a system, and can help identify which file systems are running low on space.

The `df` command typically takes one or more file system paths as arguments, and displays the amount of used and free space, as well as the total and percentage capacity of each file system. The output can also include information about the type of file system, the number of inodes, and the mount point of the file system.

The `df` command can be useful for system administrators and users who need to monitor disk usage on a system and identify potential issues with low disk space.

Designing the Typescript implementation of the df command
To implement the `df` command in Typescript, we can follow the following basic steps:

1. Parse any command line arguments to determine the file system paths to query.
2. Use the `fs.stat()` method to get information about each file system path, including the total size and available space.
3. Calculate the used and free space for each file system, as well as the percentage of total capacity that is used.
4. Format the output in a human-readable format, including information about the file system type, mount point, total size, used space, free space, and percentage used.
5. Print the output to the console.

We can use the `fs` module in Node.js to interact with the file system and get information about file system paths. We can also use the `filesize` library to format the file sizes in a human-readable format.

Reading and processing the input file system
To read and process the input file system for the `df` command, we can use the `fs.stat()` method in Node.js to get information about each file system path that was specified as a command line argument. Here's an example code snippet:

```typescript
import fs from 'fs';
import { promisify } from 'util';

const stat = promisify(fs.stat);

async function getFileSystemStats(paths: string[]) {
 const stats = await Promise.all(paths.map((path) => stat(path)));

 return stats.map((stat, index) => ({
 path: paths[index],
 totalSize: stat.blocks * stat.blksize,
 availableSpace: stat.blocksAvailable * stat.blksize
 }));
}
```

In this code, we first import the `fs` module and the `promisify` function from the `util` module to convert the `fs.stat()` method into a Promise-based function. We define an `async` function called `getFileSystemStats` that takes an array of file system paths as an argument.

Inside the function, we use `Promise.all()` to call `fs.stat()` for each file system path, and store the resulting `fs.Stats` object in an array called `stats`. We then use the `map()` method to convert each `fs.Stats` object into an object with the path, total size, and available space properties.

The `totalSize` property is calculated by multiplying the `blocks` and `blksize` properties of the `fs.Stats` object. The `availableSpace` property is calculated by multiplying the `blocksAvailable` and `blksize` properties of the `fs.Stats` object.

The function returns an array of objects, where each object represents a file system path and contains information about the total size and available space.

Implementing the df functionality
To implement the `df` functionality in Typescript, we can use the `getFileSystemStats()` function we defined earlier to get information about the file systems specified as command line arguments. We can then use this information to calculate the used and free space for each file system, as well as the percentage of total capacity that is used. Finally, we can format the output in a human-readable format and print it to the console.

Here's an example code snippet:

```typescript
import { Command } from 'commander';
import { getFileSystemStats } from './fs-utils';
import { format } from 'filesize';

const program = new Command();

program
 .option('-h, --human-readable', 'print sizes in human readable format')
 .parse(process.argv);

const paths = program.args;

async function df() {
 const fsStats = await getFileSystemStats(paths);

 fsStats.forEach((fsStat) => {
 const usedSpace = fsStat.totalSize - fsStat.availableSpace;
 const percentageUsed = (usedSpace / fsStat.totalSize) * 100;
 const sizeFormat = program.humanReadable ? format : (size: number) => size.toString();

 console.log(`${fsStat.path}\t${sizeFormat(fsStat.totalSize)}\t${sizeFormat(usedSpace)}\t${sizeFormat(fsStat.availableSpace)}\t${percentageUsed.toFixed(1)}%`);
 });
}

df();
```

In this code, we first import the `Command` class from the `commander` module, the `getFileSystemStats()` function we defined earlier, and the `format()` function from the `filesize` library to format the file sizes in a human-readable format.

We then create a new instance of the `Command` class and define an option called `-h` or `--human-readable` that determines whether the sizes should be printed in a human-readable format. We use the `parse()` method to parse the command line arguments and store any file system paths in an array called `paths`.

Next, we define an `async` function called `df()` that calls the `getFileSystemStats()` function to get information about the file systems specified as command line arguments. We then use the `forEach()` method to loop through each file system and calculate the used space, free space, and percentage used.

We use a ternary operator to determine whether to use the `format()` function to format the file sizes or to simply convert them to strings. We then use `console.log()` to print the output to the console, which includes information about the file system path, total size, used space, free space, and percentage used. The output is formatted according to whether the `-h` option was specified.

Finally, we call the `df()` function to run the command.

Outputting the results to the console
Once the calculations have been performed, we can output the results to the console. We can display the output in a tabular format to make it more readable. We can use the `console.table` method to display the output in a tabular format.

Here's the implementation:

```typescript
function printTable(rows: any[]) {
 console.table(rows, ['Filesystem', 'Size', 'Used', 'Available', 'Use%', 'Mounted on']);
}

function printResults(filesystem: string, blockSize: number, blocks: number, usedBlocks: number, availableBlocks: number) {
 const size = blockSize * blocks;
 const used = blockSize * usedBlocks;
 const available = blockSize * availableBlocks;
 const usePercent = ((used / size) * 100).toFixed(2) + '%';
 const data = {
 'Filesystem': filesystem,
 'Size': size,
 'Used': used,
 'Available': available,
 'Use%': usePercent,
 'Mounted on': '/'
 };
 printTable([data]);
}
```

In the above code, the `printTable` function takes an array of objects and displays them in a tabular format using the `console.table` method. The second parameter of `console.table` specifies the columns to display.

The `printResults` function calculates the values of `size`, `used`, `available`, and `usePercent` using the provided arguments. It then creates an object with the calculated values and passes it to the `printTable` function for display.

We can now call the `printResults` function to output the results to the console.

Testing the df command
To test the `df` command, we can run it on a file system and check if the results match the expected output.

Here's an example test case:

```typescript
import { df } from './df';

test('df command should display file system information', () => {
 const result = df('/dev', 1024);
 const expectedOutput = [
 {
 'Filesystem': '/dev',
 'Size': 20971520,
 'Used': 1241088,
 'Available': 19730432,
 'Use%': '5.92%',
 'Mounted on': '/'
 }
 ];
 expect(result).toEqual(expectedOutput);
});
```

In the above code, we import the `df` function from the `df.ts` module. We then define a test case that calls the `df` function with a file system path and a block size of `1024`. We then define the expected output as an array of objects with the file system information.

Finally, we use the `expect` function to compare the actual output of the `df` command with the expected output. If the actual output matches the expected output, the test case passes. If not, it fails.

We can run this test case using a testing framework like Jest to ensure that the `df` command works correctly.

# Chapter 6: Building a Rm Command

Overview of the rm command and its functionality
The `rm` (remove) command is a Unix/Linux utility that is used to remove files or directories. It can be used to remove a single file, multiple files, or an entire directory tree.

The basic syntax of the `rm` command is:

```typescript
rm [options] file(s) or directory
```

Some common options for the `rm` command include:

* `-r`: Removes directories and their contents recursively
* `-f`: Forces the removal of files without prompting for confirmation
* `-i`: Prompts the user before deleting each file

The `rm` command can be a powerful tool, so it should be used with care to avoid accidentally deleting important files or directories.

Designing the Typescript implementation of the rm command
To design the Typescript implementation of the `rm` command, we need to first decide on the input and output of the command. The `rm` command takes one or more file or directory paths as input and removes them. It does not produce any output.

Here is a basic design for the `rm` command:

```typescript
rm(path: string[], recursive: boolean, force: boolean, prompt: boolean): void
```

The `rm` function takes an array of file or directory paths to remove, and three boolean parameters:

* `recursive`: If `true`, the function will remove directories and their contents recursively.
* `force`: If `true`, the function will remove files without prompting for confirmation.
* `prompt`: If `true`, the function will prompt the user before deleting each file.

The `rm` function does not return anything. It will throw an error if there is a problem removing any of the files or directories.

Here is a more detailed design for the `rm` command:

```typescript
function rm(path: string[], recursive: boolean, force: boolean, prompt: boolean): void {
 // Validate input
 // - Ensure path exists and is a file or directory
 // - Ensure user has permission to delete path

 // If path is a file, delete it
 // If path is a directory and recursive flag is set, delete it and its contents recursively
 // If path is a directory and recursive flag is not set, throw an error

 // If force flag is set, delete files without prompting for confirmation
 // If prompt flag is set, prompt user before deleting each file
}
```

This design outlines the basic steps that the `rm` function will take to remove files and directories. It also includes input validation to ensure that the user has provided valid paths to remove and has permission to delete them. The function will handle files and directories differently based on the `recursive` flag, and will either delete them or throw an error if necessary. Finally, the function will handle the `force` and `prompt` flags to either delete files without prompting or to prompt the user before deleting each file.

Reading and processing the input file or directory
To implement the `rm` command in Typescript, we first need to read and process the input file or directory path provided by the user. We can use the `fs` module in Node.js to achieve this.

Here's an updated version of the design that includes the file and directory processing step:

```typescript
function rm(path: string[], recursive: boolean, force: boolean, prompt: boolean): void {
 // Validate input
 // - Ensure path exists and is a file or directory
 // - Ensure user has permission to delete path

 // Read and process input files and directories
 const pathsToDelete: string[] = [];

 path.forEach((p) => {
 if (fs.existsSync(p)) {
 const stats = fs.statSync(p);
 if (stats.isDirectory()) {
 if (recursive) {
 pathsToDelete.push(p);
 } else {
 throw new Error(`Cannot delete directory '${p}'. Use the recursive flag to delete directories.`);
 }
 } else if (stats.isFile()) {
 pathsToDelete.push(p);
 } else {
 throw new Error(`Path '${p}' is not a file or directory.`);
 }
 } else {
 throw new Error(`Path '${p}' does not exist.`);
 }
 });

 // If path is a file, delete it
 // If path is a directory and recursive flag is set, delete it and its contents recursively
 // If path is a directory and recursive flag is not set, throw an error

 // If force flag is set, delete files without prompting for confirmation
 // If prompt flag is set, prompt user before deleting each file
}
```

In this updated design, we first use the `fs` module to check if each path in the input array exists, and if so, whether it is a file or directory. If a path is a file, we add it to an array of paths to delete. If a path is a directory, we check whether the `recursive` flag is set, and add it to the array of paths to delete if it is. If `recursive` is not set for a directory, we throw an error.

We use the `fs` module's `existsSync` method to check if the path exists, and the `fs` module's `statSync` method to get information about the path. We can then use the `isDirectory` and `isFile` methods of the `fs.Stats` object to determine if the path is a directory or a file.

Once we have processed the input files and directories, we can move on to deleting them based on the `recursive`, `force`, and `prompt` flags.

Implementing the rm functionality
To implement the `rm` command in Typescript, we can use the `fs` module to delete the specified file or directory. The implementation will take the following steps:

1. Check if the specified path exists and is a file or directory.
2. If it is a file, delete it using the `fs.unlink` method.
3. If it is a directory, recursively delete all its contents and then delete the directory itself using the `fs.rmdir` method.

Here's the Typescript code for the `rm` command implementation:

```typescript
import fs from 'fs';
import path from 'path';

function deleteFileOrDirectory(filePath: string) {
 const stats = fs.statSync(filePath);

 if (stats.isFile()) {
 fs.unlinkSync(filePath);
 } else if (stats.isDirectory()) {
 fs.readdirSync(filePath).forEach((file) => {
 const curPath = path.join(filePath, file);
 deleteFileOrDirectory(curPath);
 });
 fs.rmdirSync(filePath);
 }
}

function main() {
 const args = process.argv.slice(2);

 if (args.length === 0) {
 console.error('Usage: rm <file or directory>');
 process.exit(1);
 }

 const filePath = args[0];

 if (!fs.existsSync(filePath)) {
 console.error(`Error: ${filePath} does not exist`);
 process.exit(1);
 }

 deleteFileOrDirectory(filePath);
}

main();
```

This implementation recursively deletes all contents of a specified directory. It also checks if the specified file or directory exists before attempting to delete it, and displays an error message if it does not exist.

Note that the `fs.unlinkSync` method is used to delete a file synchronously, while the `fs.rmdirSync` method is used to delete a directory synchronously. The `fs.readdirSync` method is used to get a list of all files and directories in a specified directory.

Outputting the results to the console
Since the `rm` command does not produce any output, we don't need to output anything to the console. However, we should display an error message if the specified file or directory cannot be deleted.

We can modify the `deleteFileOrDirectory` function to throw an error if it encounters any issues while deleting a file or directory. Then, we can catch this error in the `main` function and display an error message to the console.

Here's the updated code for the `deleteFileOrDirectory` function:

```typescript
function deleteFileOrDirectory(filePath: string) {
 const stats = fs.statSync(filePath);

 if (stats.isFile()) {
 fs.unlinkSync(filePath);
 } else if (stats.isDirectory()) {
 fs.readdirSync(filePath).forEach((file) => {
 const curPath = path.join(filePath, file);
 deleteFileOrDirectory(curPath);
 });
 fs.rmdirSync(filePath);
 } else {
 throw new Error(`${filePath} is not a file or directory`);
 }
}
```

And here's the updated `main` function:

```typescript
function main() {
 const args = process.argv.slice(2);

 if (args.length === 0) {
 console.error('Usage: rm <file or directory>');
 process.exit(1);
 }

 const filePath = args[0];

 if (!fs.existsSync(filePath)) {
 console.error(`Error: ${filePath} does not exist`);
 process.exit(1);
 }

 try {
 deleteFileOrDirectory(filePath);
 } catch (error) {
 console.error(`Error: ${error.message}`);
 process.exit(1);
 }
}
```

Now, if an error occurs while deleting a file or directory, we display an error message to the console.

Testing the rm command
To test the `rm` command, we can create some files and directories and then delete them using the `rm` command.

Here's an example of how we can test the `rm` command:

1. Create a new directory called `test_dir` and navigate into it:

```typescript
mkdir test_dir
cd test_dir
```

2. Create some files and directories inside the `test_dir` directory:

```typescript
mkdir dir1
touch file1.txt
touch file2.txt
mkdir dir2
touch file3.txt
```

3. Use the `ls` command to confirm that the files and directories were created:

```typescript
$ ls
dir1 dir2 file1.txt file2.txt file3.txt
```

4. Use the `rm` command to delete the `file1.txt` file:

```typescript
rm file1.txt
```

The `file1.txt` file should be deleted.
5. Use the `ls` command to confirm that the `file1.txt` file was deleted:

```typescript
$ ls
dir1 dir2 file2.txt file3.txt
```

6. Use the `rm` command to delete the `dir1` directory and its contents:

```typescript
rm -r dir1
```

The `dir1` directory and its contents should be deleted.
7. Use the `ls` command to confirm that the `dir1` directory and its contents were deleted:

```typescript
$ ls
dir2 file2.txt file3.txt
```

8. Use the `rm` command to delete the `test_dir` directory and its contents:

```typescript
cd ..
rm -r test_dir
```

The `test_dir` directory and its contents should be deleted.
9. Use the `ls` command to confirm that the `test_dir` directory and its contents were deleted:

```typescript
ls
```

There should be no output, indicating that the `test_dir` directory and its contents were successfully deleted.

# Chapter 7: Building a Touch Command

Overview of the touch command and its functionality
The `touch` command is a command-line utility in Unix and Unix-like operating systems, used to create, change and modify timestamps of a file. If the file specified does not exist, it creates an empty file with the name provided. If the file exists, the timestamp of the file is updated to the current time. The name of the command `touch` comes from the fact that it updates the timestamp of the file and thus "touches" the file.

Designing the Typescript implementation of the touch command
The implementation of the `touch` command in Typescript will require the following steps:

1. Reading and processing the command line arguments to determine the file name(s) to touch and the options specified (such as `-c`, `-a`, `-m`).
2. Checking if the file(s) specified exist or not. If it doesn't exist, create an empty file with the specified name. If it does exist, update the access and/or modification timestamp of the file as specified by the options.
3. Output the result to the console.

We will need to use the `fs` module in Node.js to interact with the file system and modify the timestamps of the files.

Here is a sample implementation in Typescript:

```typescript
import fs from 'fs';

interface TouchOptions {
 c: boolean;
 m: boolean;
 a: boolean;
}

function touchFile(filename: string, options: TouchOptions): void {
 if (fs.existsSync(filename)) {
 if (options.a) {
 fs.utimesSync(filename, new Date(), new Date());
 } else if (options.m) {
 fs.utimesSync(filename, new Date(), fs.statSync(filename).atime);
 } else {
 fs.utimesSync(filename, fs.statSync(filename).atime, new Date());
 }
 } else {
 fs.writeFileSync(filename, '');
 }
}

function touchFiles(filenames: string[], options: TouchOptions): void {
 for (const filename of filenames) {
 touchFile(filename, options);
 console.log(`Updated timestamp of file: ${filename}`);
 }
}

function parseArguments(args: string[]): { files: string[]; options: TouchOptions } {
 const options: TouchOptions = { c: false, m: false, a: false };
 const files: string[] = [];

 for (let i = 0; i < args.length; i++) {
 const arg = args[i];
 if (arg.startsWith('-')) {
 for (const option of arg.slice(1)) {
 options[option as keyof TouchOptions] = true;
 }
 } else {
 files.push(arg);
 }
 }

 return { files, options };
}

function main(args: string[]): void {
 const { files, options } = parseArguments(args);
 touchFiles(files, options);
}

main(process.argv.slice(2));
```

In this implementation, we define an `interface` for the `TouchOptions` that contains the boolean flags for `-c`, `-m`, and `-a`. We define a `touchFile` function that takes in a filename and the options and updates the timestamps of the file. If the file doesn't exist, it creates a new empty file. We define a `touchFiles` function that takes in an array of filenames and options, and calls `touchFile` on each of them. We also define a `parseArguments` function that parses the command-line arguments and returns the options and filenames. Finally, we define a `main` function that calls `parseArguments` and `touchFiles` with the options and filenames respectively.

Reading and processing the input file
Since the `touch` command creates a new file or updates the modification time of an existing file, it needs to read and process the input file.

The input file can be specified as a command-line argument, and the touch command needs to determine if the file already exists or needs to be created. If the file already exists, the touch command should update its modification time. If the file doesn't exist, the touch command should create it.

In addition, the touch command may need to create intermediate directories if they don't already exist, depending on the path specified for the new file.

To read and process the input file, the touch command can use the `fs` module provided by Node.js. Specifically, it can use the `fs.stat()` method to check if the file already exists, and the `fs.writeFile()` method to create a new file or update the modification time of an existing file. The `fs.mkdir()` method can also be used to create intermediate directories if needed.

Implementing the touch functionality
To implement the touch functionality in TypeScript, we can start by importing the `fs` module:

```typescript
import * as fs from 'fs';
```

Then, we can define a function that takes a file path as a parameter and updates its modification time or creates the file if it doesn't exist. We can use the `fs.stat()` method to check if the file exists, and the `fs.writeFile()` method to create the file or update its modification time. Here's an example implementation:

```typescript
function touch(filePath: string): void {
 fs.stat(filePath, (err, stats) => {
 if (err) {
 if (err.code === 'ENOENT') {
 // File doesn't exist, create it
 fs.writeFile(filePath, '', err => {
 if (err) {
 console.error(`Error creating file: ${err}`);
 } else {
 console.log(`Created file: ${filePath}`);
 }
 });
 } else {
 console.error(`Error checking file: ${err}`);
 }
 } else {
 // File already exists, update its modification time
 fs.utimes(filePath, stats.atime, new Date(), err => {
 if (err) {
 console.error(`Error updating file: ${err}`);
 } else {
 console.log(`Updated file: ${filePath}`);
 }
 });
 }
 });
}
```

This function checks if the file exists by calling `fs.stat()` with the file path. If the file doesn't exist, the function creates it by calling `fs.writeFile()` with an empty string as the file content. If the file already exists, the function updates its modification time by calling `fs.utimes()` with the existing access time and the current time.

Finally, we can call this `touch()` function with the file path specified as a command-line argument:

```typescript
const filePath = process.argv[2];
touch(filePath);
```

Note that this implementation doesn't handle the case where the file path specifies intermediate directories that don't exist. In that case, we need to create the intermediate directories before creating the file. We can use the `fs.mkdir()` method for this.

Outputting the results to the console
The `touch` command does not typically have any output to the console unless there is an error. If the file already exists, it will update the modification time, and if it doesn't exist, it will create an empty file with the specified name. A message can be displayed if the file is successfully created or updated.

Testing the touch command
To test the `touch` command, you can perform the following steps:

1. Create a test file: `touch testfile.txt`
2. Verify that the file was created: `ls -l testfile.txt`
3. Check the modification time of the file: `stat testfile.txt`
4. Update the file's modification time: `touch testfile.txt`
5. Verify that the modification time has been updated: `stat testfile.txt`
6. Try creating a file with an invalid file name: `touch $'invalid:file:name'`

By performing these steps, you can verify that the `touch` command is able to create and update files with valid file names and that it fails gracefully when provided with invalid file names.

# Chapter 8: Building a Locate Command

Overview of the locate command and its functionality
The `locate` command is used to find files on the system by name. It works by searching an index of file names and locations, which is created and updated periodically by the system's indexing service. The `locate` command then searches this index for files that match the specified search pattern and outputs their paths to the console. The `locate` command is typically much faster than using `find` to search the file system, especially when searching for files with common names. However, it may not find files that have been recently added or modified, since the index is only updated periodically.

Designing the Typescript implementation of the locate command
To implement the `locate` command in Typescript, we will need to use the Node.js `child_process` module to run the `locate` command in a new child process and capture its output. Here is an outline of the steps involved:

1. Parse the command line arguments to determine the search pattern and any options, such as case sensitivity or file type filters.
2. Construct the `locate` command string based on the options specified and the search pattern.
3. Use the `child_process.spawn()` method to create a new child process that runs the `locate` command with the specified options and search pattern.
4. Capture the output of the `locate` command as it is streamed to the console by listening to the `stdout` event of the child process.
5. Parse the output of the `locate` command to extract the file paths that match the search pattern.
6. Output the matching file paths to the console in a formatted way.

We can implement each of these steps in Typescript to create a working `locate` command.

Reading and processing the input search query
To implement the `locate` command in Typescript, we need to first read and process the input search query. The `locate` command accepts a single argument which is the search query. The command then searches the entire file system for files and directories that contain the search query in their name or path.

In Typescript, we can read the input search query using the `process.argv` array. The first argument in the `process.argv` array is the path to the `node` executable, and the second argument is the path to the Typescript file being executed. We can access the input search query by getting the third argument in the array, which corresponds to the search query.

Here's some sample code to read and process the input search query:

```typescript
const searchQuery = process.argv[2];
```

This code reads the input search query from the command line arguments and stores it in the `searchQuery` variable.

Next, we need to check if the input search query is valid. If the search query is not provided, we should display an error message and exit the program. If the search query is provided, we can proceed with searching the file system for files and directories that contain the search query in their name or path.

Implementing the locate functionality
To implement the locate command, we can use the `find` command that comes with most Unix-like systems. The `find` command recursively searches for files and directories in a given path, based on certain criteria, such as the file name, size, or modification time.

In our Typescript implementation of the `locate` command, we will take the following steps:

1. Parse the input search query, which can be a string or a regular expression.
2. Use the `child_process` module to execute the `find` command with the given path and search criteria.
3. Parse the output of the `find` command, which will be a list of file paths.
4. Output the list of file paths to the console.

Let's go through each of these steps in more detail.

First, we need to parse the input search query. The search query can be a string or a regular expression, so we need to handle both cases. We can define a function called `parseSearchQuery` that takes a string as input and returns either a string or a regular expression, depending on the format of the input. For example, if the input is `/usr/local/*.txt`, we will return a regular expression that matches all files with a `.txt` extension in the `/usr/local` directory. If the input is just `*.txt`, we will return a string that matches all files with a `.txt` extension in the current directory.

Next, we will use the `child_process` module to execute the `find` command with the given path and search criteria. We can define a function called `executeFindCommand` that takes a path and a search query as input and returns a Promise that resolves with the output of the `find` command. To execute the `find` command, we can use the `spawn` method of the `child_process` module, passing the path and search query as arguments.

Once we have the output of the `find` command, we need to parse it to get a list of file paths. We can define a function called `parseFindOutput` that takes the output of the `find` command as input and returns an array of file paths. To do this, we can split the output by newline characters and filter out any empty lines or lines that don't contain file paths.

Finally, we need to output the list of file paths to the console. We can define a function called `outputFilePaths` that takes an array of file paths as input and outputs each path on a separate line.

With these functions in place, we can define our `locate` command by calling the `parseSearchQuery`, `executeFindCommand`, and `parseFindOutput` functions in sequence, and then passing the resulting array of file paths to the `outputFilePaths` function.

Outputting the results to the console
To output the results of the `locate` command to the console, we can simply loop through the results array and print each item to the console.

Here's an example implementation:

```typescript
function outputLocateResults(results: string[], humanReadable: boolean): void {
 if (results.length === 0) {
 console.log('No results found.');
 return;
 }

 console.log('Results:');

 results.forEach((result) => {
 console.log(result);
 });

 if (humanReadable) {
 console.log('Total number of results:', results.length);
 }
}
```

In this implementation, we first check if the results array is empty. If it is, we print a message to the console and return. If there are results, we print a message to indicate that the results are about to be printed.

We then loop through the results array and print each item to the console using `console.log()`. Finally, if the `humanReadable` flag is set to true, we print the total number of results to the console.

Testing the locate command
To test the `locate` command, we can create some test files in a directory and then search for them using the command.

First, let's create a directory called `testDir` and move into it:

```typescript
fs.mkdirSync("testDir");
process.chdir("testDir");
```

Now, we'll create some test files using the `touch` command:

```typescript
fs.writeFileSync("file1.txt", "");
fs.writeFileSync("file2.txt", "");
fs.writeFileSync("file3.md", "");
```

Finally, we can test the `locate` command by searching for all files with the `.txt` extension:

```typescript
const searchQuery = ".txt";
const results = await locate(searchQuery);
console.log(results);
```

This should output an array containing the paths to the `file1.txt` and `file2.txt` files.

# Chapter 9: Building a Find Command

Overview of the find command and its functionality
The `find` command is a powerful tool that is used to search for files and directories within a specified directory hierarchy. It can be used to search for files based on different criteria, such as name, size, modification time, and permissions. The `find` command is commonly used by system administrators to manage and maintain file systems. It can also be used by developers to locate specific files in a project directory.

Designing the Typescript implementation of the find command
The `find` command in Typescript can be implemented using the `fs` module provided by Node.js. The basic functionality of the `find` command is to search for files in a specified directory hierarchy based on different search criteria.

Here is a rough outline of the design for the Typescript implementation of the `find` command:

1. Parse the command line arguments to extract the search directory and the search criteria.
2. Check if the search directory exists, and if it is a directory or a file.
3. If the search directory is a file, output an error message and exit.
4. If the search directory is a directory, recursively search for files based on the search criteria.
5. For each file found, output the file path to the console.

To implement the search criteria, we can use the `fs.stat()` method to get the file stats and check the file name, size, modification time, or permissions.

We can also use the `path` module to handle file path operations like joining and normalizing paths.

Overall, the implementation of the `find` command in Typescript would involve working with file system operations provided by Node.js, parsing command line arguments, and outputting search results to the console.

Reading and processing the input directory and search query
To implement the `find` command, we need to read and process the input directory path and the search query pattern.

The input directory path is specified as the first argument after the command name. We can use the `process.argv` array to retrieve the command line arguments, where `process.argv[0]` is the path to the Node.js executable, `process.argv[1]` is the path to the script file, and the remaining elements are the command line arguments.

The search query pattern is specified as an option, which can be preceded by a hyphen (`-`) or double hyphen (`--`). For example, to search for files with the `.txt` extension, we can use the `-name "*.txt"` option. We can use the `minimist` package to parse the command line arguments and retrieve the options.

Here's an example code snippet to retrieve the input directory path and search query pattern:

```typescript
import minimist from 'minimist';
import { resolve } from 'path';

const args = minimist(process.argv.slice(2));
const directory = resolve(args._[0] || '.'); // use current directory if not specified
const query = args.name ? `-name "${args.name}"` : '';
```

In this example, we're using the `resolve` function from the `path` module to resolve the input directory path to an absolute path, and defaulting to the current directory if not specified. We're also checking if the `name` option is specified, and using it to construct the search query pattern.

Implementing the find functionality
To implement the `find` functionality in Typescript, we can follow these steps:

1. Read the input directory and search query from the command-line arguments.
2. Use the `fs` module to traverse the directory recursively and find all files and subdirectories.
3. Use the `path` module to check if each file matches the search query.
4. Print the absolute path of each matching file to the console.

Here's an implementation of the `find` command in Typescript:

```typescript
import fs from 'fs';
import path from 'path';

function findFiles(dir: string, query: string): string[] {
 let results: string[] = [];

 fs.readdirSync(dir).forEach((file) => {
 const filePath = path.join(dir, file);

 if (fs.statSync(filePath).isDirectory()) {
 results = results.concat(findFiles(filePath, query));
 } else if (file.includes(query)) {
 results.push(filePath);
 }
 });

 return results;
}

const [dir, query] = process.argv.slice(2);
const files = findFiles(dir, query);

console.log(files.join('\n'));
```

In this implementation, the `findFiles` function is called recursively for each subdirectory of the input directory. It checks if each file in the directory matches the search query, and if so, adds its absolute path to the `results` array. Finally, the function returns the `results` array, which contains the absolute paths of all matching files.

In the main body of the program, we read the input directory and search query from the command-line arguments, and call the `findFiles` function to find all matching files. We then print the absolute path of each matching file to the console, separated by newline characters.

Note that this implementation does not handle errors that may occur while reading the input directory, such as permission errors or non-existent directories. It also does not handle edge cases such as file names containing newlines. These issues can be addressed with additional error handling and input validation.

Outputting the results to the console
To output the results of the find command to the console, we can use the `console.log()` method to print each matching file path to the console. We can also provide options to customize the output, such as showing only the file name instead of the full path, and displaying the file size or modification date.

Here is an example implementation of the `find` command's output functionality:

```typescript
interface FindOptions {
 pathOnly?: boolean;
 showSize?: boolean;
 showDate?: boolean;
}

function printFileDetails(filePath: string, options: FindOptions): void {
 const stats = fs.statSync(filePath);

 let output = filePath;
 if (options.pathOnly) {
 output = path.basename(filePath);
 }

 if (options.showSize) {
 output += ` (${stats.size} bytes)`;
 }

 if (options.showDate) {
 output += ` (${stats.mtime.toLocaleString()})`;
 }

 console.log(output);
}

function printMatches(matches: string[], options: FindOptions): void {
 for (const filePath of matches) {
 printFileDetails(filePath, options);
 }
}
```

The `printFileDetails()` function takes in a file path and the options object, and constructs the output string based on the selected options. The `printMatches()` function takes in the list of matching file paths and the options object, and prints each file's details to the console using the `printFileDetails()` function.

Note that the `fs.statSync()` method is used to retrieve the file's size and modification date. The `path.basename()` method is used to extract the file name from the full path, if the `pathOnly` option is enabled.

We can call the `printMatches()` function with the list of matching file paths and the options object, as follows:

```typescript
const matches = findFiles(directoryPath, searchQuery);
printMatches(matches, { pathOnly: true, showSize: true });
```

This will print the list of matching file names along with their sizes, using a human-readable format if the file size is large.

Testing the find command
As with the other commands, we will need to test the `find` command thoroughly to ensure that it works as expected. We can create a directory structure with files and directories to use for testing the command.

Here's an example of how we can set up our test directory structure:

```typescript
testDir
├── file1.txt
├── file2.txt
├── file3.md
├── subDir1
│ ├── file4.txt
│ ├── file5.md
│ └── subDir2
│ ├── file6.txt
│ └── file7.md
└── subDir3
 ├── file8.txt
 ├── file9.md
 └── subDir4
 ├── file10.txt
 └── file11.md
```

We can create the test directory structure using the following commands:

```typescript
mkdir testDir
cd testDir
touch file1.txt file2.txt file3.md
mkdir subDir1 subDir3
cd subDir1
touch file4.txt file5.md
mkdir subDir2
cd subDir2
touch file6.txt file7.md
cd ../../subDir3
touch file8.txt file9.md
mkdir subDir4
cd subDir4
touch file10.txt file11.md
cd ../..
```

Now, we can test the `find` command by running it with different search queries and options, and verifying that the output is correct. Here are some examples of how we can test the command:

```typescript

# Find all files in the test directory structure
ts-node find.ts testDir


# Find all files with .txt extension in the test directory structure
ts-node find.ts testDir -name "\*.txt"


# Find all files with .md extension in the subDir1 directory
ts-node find.ts testDir/subDir1 -name "\*.md"


# Find all files with .txt extension in the subDir2 directory
ts-node find.ts testDir/subDir1/subDir2 -name "\*.txt"


# Find all directories in the test directory structure
ts-node find.ts testDir -type d


# Find all files in the test directory structure modified in the last 24 hours
ts-node find.ts testDir -mtime 0
```

We should compare the output of the `find` command with the expected results for each test case, and make sure that they match. If the output is correct for all test cases, we can conclude that the `find` command works as expected.

# Chapter 10: Building a Grep Command

Overview of the grep command and its functionality
The `grep` command is a powerful search tool used to find specific text or patterns in files or directories. It stands for "Global Regular Expression Print". The command takes in a pattern, searches for it in a given input file, and outputs any lines that contain the specified pattern.

The basic syntax of the `grep` command is as follows:

```typescript
grep pattern file
```

Where `pattern` is the text or regular expression to search for, and `file` is the name of the file to search in. Multiple files can be searched by using a wildcard, for example:

```typescript
grep pattern *.txt
```

This would search for the pattern in all text files in the current directory.

The `grep` command also supports various options that modify its behavior, such as case sensitivity, recursive searching, and displaying line numbers.

Designing the Typescript implementation of the grep command
The `grep` command is used to search for specific patterns in files. The command takes a regular expression pattern and a file name or names as arguments, and searches each specified file for lines containing the pattern.

To design the Typescript implementation of the `grep` command, we need to consider the following steps:

1. Reading and processing the input file or files
2. Searching for the specified pattern in each file
3. Outputting the matching lines to the console

To handle the regular expression pattern, we can use the built-in `RegExp` object in Typescript.

To handle multiple files, we can accept an array of file names as input, and iterate through each file to search for the pattern.

To handle command line options, we can use a library like `commander` to parse and handle the options. Possible options for the `grep` command include:

* `-i`: ignore case when searching for the pattern
* `-n`: print the line numbers of the matching lines
* `-v`: invert the match, printing only the lines that do not match the pattern
* `-c`: print only the count of matching lines

By implementing these steps and options, we can create a Typescript version of the `grep` command that can be used to search for patterns in files.

Reading and processing the input file and search query
To implement the `grep` command in TypeScript, we need to read and process the input file and search query.

We can start by accepting the search query as the first argument, and the file path as the second argument. We will then read the file line by line and search for the given pattern in each line. If a line matches the pattern, we will store it in a result array.

Here's the basic code skeleton for reading and processing the input file:

```typescript
import * as fs from 'fs';

const searchQuery = process.argv[2];
const filePath = process.argv[3];

const fileData = fs.readFileSync(filePath, 'utf-8');
const fileLines = fileData.split('\n');

const result = [];
for (const line of fileLines) {
 if (line.includes(searchQuery)) {
 result.push(line);
 }
}
```

In this code, we first read the search query and file path from the command line arguments. We then read the contents of the file and split it into an array of lines. We iterate through each line and check if it contains the search query using the `includes()` method. If it does, we add the line to the result array.

Note that this is just the basic structure, and we will need to add error handling and other features for a complete implementation.

Implementing the grep functionality
To implement the grep functionality in Typescript, we need to read the input file and search for the specified pattern within it. Here are the basic steps:

1. Read the input file and store its content in a string variable.
2. Split the content of the file into an array of lines.
3. Loop through each line of the file, searching for the specified pattern using a regular expression.
4. If the pattern is found in the line, output the line to the console.

Here's a sample implementation of the `grep` command in Typescript:

```typescript
import * as fs from 'fs';

function grep(filename: string, pattern: string) {
 const content = fs.readFileSync(filename, 'utf8');
 const lines = content.split('\n');

 for (let i = 0; i < lines.length; i++) {
 const line = lines[i];
 const match = line.match(pattern);
 if (match) {
 console.log(line);
 }
 }
}
```

In this implementation, we use the Node.js `fs` module to read the input file and store its content in the `content` variable. We then split the content of the file into an array of lines using the `split` method. We then loop through each line of the file using a `for` loop, searching for the specified pattern using the `match` method of the `String` class, which returns an array of matches or null if no match is found. If a match is found, we output the line to the console using the `console.log` method.

Note that this implementation assumes that the input file is a text file. If the input file is a binary file, we would need to use a different approach to read its content.

Outputting the results to the console
To output the results of the `grep` command to the console, we can simply loop through the `matchingLines` array and log each line to the console using `console.log()`:

```typescript
for (const line of matchingLines) {
 console.log(line);
}
```

We can also add an option to output the results to a file by using the `fs` module to create a write stream and piping the matching lines to it:

```typescript
if (outputFile) {
 const writeStream = fs.createWriteStream(outputFile);
 for (const line of matchingLines) {
 writeStream.write(line + '\n');
 }
 writeStream.end();
} else {
 for (const line of matchingLines) {
 console.log(line);
 }
}
```

In the above code, we first check if an output file was specified using the `outputFile` variable. If it was, we create a write stream using `fs.createWriteStream()` and loop through the `matchingLines` array, writing each line to the stream. Finally, we call `writeStream.end()` to close the stream.

If no output file was specified, we simply loop through the `matchingLines` array and log each line to the console using `console.log()`.

Testing the grep command
To test the `grep` command, we can create a sample text file and search for a specific string in it.

Here's an example:

1. Create a file named `sample.txt` and add some text to it.

```typescript
echo "This is a sample text file. It contains some random text." > sample.txt
```

2. Run the `grep` command to search for a specific string.

```typescript
grep "random" sample.txt
```

This should output the line that contains the word "random" in the `sample.txt` file.

```typescript
It contains some random text.
```

# Chapter 11: Building a Tar Command

Overview of the tar command and its functionality
The `tar` command is used to create, view, and extract tar archives. A tar archive is a collection of files that are stored as a single file. The tar format is often used to create backups or to distribute files.

The `tar` command can be used to perform a variety of operations on tar archives, including:

* `c`: Create a new tar archive.
* `x`: Extract files from a tar archive.
* `t`: List the contents of a tar archive.
* `u`: Update an existing tar archive.
* `r`: Append files to an existing tar archive.
* `f`: Specify the name of the tar archive.

The `tar` command can also be used with various options to customize its behavior. For example, the `-z` option is used to compress the archive with gzip, and the `-j` option is used to compress the archive with bzip2.

Designing the Typescript implementation of the tar command
To implement the `tar` command in TypeScript, we can follow these steps:

1. Parse the command line arguments to determine the mode of operation, i.e., whether to create an archive or extract files from an archive.
2. If the mode of operation is to create an archive, determine the files/directories to be included in the archive.
3. If the mode of operation is to extract files from an archive, determine the archive file and the target directory for extraction.
4. Create an archive or extract files based on the mode of operation and the inputs provided.

We can use the `tar` library to create and extract tar archives. The library provides methods to create an archive, extract files from an archive, add files to an existing archive, and update an archive.

We can use the `yargs` library to parse command-line arguments. We can define command-line options to specify the mode of operation, archive file name, target directory, and files/directories to be included in the archive.

Once we have the necessary inputs, we can use the `tar` library to create or extract the archive. We can display progress messages and errors to the console based on the outcome of the operation.

Reading and processing the input files or directories
To implement the `tar` command, we need to be able to read and process the input files or directories.

The `tar` command is used to create archives, which are collections of files and directories that can be compressed for easier transfer or storage. To create an archive with `tar`, we need to specify the files and directories that we want to include, as well as the name of the archive file that we want to create. We can also specify various options to control the compression and other aspects of the archive creation process.

To read and process the input files or directories, we can use the `fs` module in Node.js. We can use the `fs.readdir()` method to get a list of files and directories in a directory, and the `fs.stat()` method to get information about a file or directory, such as its size and type.

We can also use the `path` module to work with file paths, and the `stream` module to handle the reading and writing of data.

Implementing the tar functionality
To implement the `tar` command, we will first need to decide on the specific tar file format we want to use. The most common formats are `tar`, `tar.gz`, and `tar.bz2`.

Once we have decided on the format, we will need to use a third-party library or module that supports that format to create the tar archive. We will also need to implement the ability to extract the contents of the tar archive.

Here's a rough outline of how we could implement the `tar` command:

1. Read in the list of files or directories to include in the tar archive.
2. Use a third-party library or module to create the tar archive in the desired format.
3. If the `--extract` flag is present, extract the contents of the tar archive using the same library or module.
4. Output the results to the console.

We can use the `tar` module available in Node.js to create and extract tar archives. We will also use the `fs` module to read and write files.

Here's a sample implementation of the `tar` command:

```typescript
import fs from "fs";
import { createReadStream, createWriteStream } from "fs";
import { createGzip, createBrotliCompress } from "zlib";
import tar from "tar";

function tarFiles(files: string[], tarFileName: string, format: "tar" | "gz" | "bz2"): void {
 const writeStream = format === "tar"
 ? createWriteStream(tarFileName)
 : format === "gz"
 ? createWriteStream(tarFileName).pipe(createGzip())
 : createWriteStream(tarFileName).pipe(createBrotliCompress());

 const pack = tar.create({ cwd: "/", follow: true, strict: true });

 pack.pipe(writeStream);

 for (const file of files) {
 pack.add(file);
 }

 pack.end(() => {
 console.log(`Successfully created tar archive at ${tarFileName}`);
 });
}

function extractTarArchive(tarFileName: string, extractPath: string): void {
 const readStream = tarFileName.endsWith(".gz")
 ? createReadStream(tarFileName).pipe(createGunzip())
 : tarFileName.endsWith(".bz2")
 ? createReadStream(tarFileName).pipe(createBrotliDecompress())
 : createReadStream(tarFileName);

 const extract = tar.extract({ cwd: extractPath, strict: true });
 readStream.pipe(extract);

 extract.on("finish", () => {
 console.log(`Successfully extracted tar archive to ${extractPath}`);
 });
}

function main(): void {
 const [, , flag, tarFileName, ...files] = process.argv;
 const format = tarFileName.endsWith(".gz") ? "gz" : tarFileName.endsWith(".bz2") ? "bz2" : "tar";

 if (flag === "--extract") {
 extractTarArchive(tarFileName, ".");
 } else {
 tarFiles(files, tarFileName, format);
 }
}

main();
```

In this implementation, we use the `tar.create()` method to create a new tar stream and the `tar.extract()` method to extract the contents of a tar archive. We also use the `zlib` module to support gzip and brotli compression formats.

To create a tar archive, we can use the `tar` module in Node.js. This module provides an easy-to-use API for creating, extracting, and manipulating tar archives.

To add files or directories to the tar archive, we can use the `tar.create` method. Here is an example of how we can create a tar archive:

```typescript
import * as tar from 'tar';
import * as fs from 'fs';

const files = ['file1.txt', 'file2.txt', 'dir1', 'dir2'];

const tarStream = tar.create({
 file: 'archive.tar',
 cwd: '/path/to/source/dir',
 gzip: true,
 filter: (path) => {
 // Exclude node_modules directory
 return !path.includes('node_modules');
 },
});

// Add files and directories to the tar archive
for (const file of files) {
 tarStream.add(file);
}

// Close the tar archive stream
tarStream.end();
```

In this example, we use the `tar.create` method to create a writable stream for the tar archive. The `file` option specifies the name of the tar archive file to create, and the `cwd` option specifies the base directory for the files and directories to add to the archive.

We also enable gzip compression by setting the `gzip` option to `true`, and we filter out the `node_modules` directory by using the `filter` option.

Next, we use a `for` loop to add each file and directory to the tar archive using the `tarStream.add` method.

Finally, we close the tar archive stream by calling the `tarStream.end` method.

To extract files from a tar archive, we can use the `tar.extract` method. Here is an example of how we can extract a tar archive:

```typescript
import * as tar from 'tar';

const tarStream = fs.createReadStream('archive.tar').pipe(tar.extract({
 cwd: '/path/to/extract/dir',
}));

tarStream.on('end', () => {
 console.log('Tar archive extraction complete.');
});
```

In this example, we use the `fs.createReadStream` method to create a readable stream for the tar archive file, and we pipe the stream to the `tar.extract` method to extract the files and directories to the specified directory.

We also use the `cwd` option to specify the base directory for the extracted files and directories.

Finally, we listen to the `end` event of the `tarStream` object to be notified when the tar archive extraction is complete.

Note that the `tar` module also supports other compression formats, such as brotli, by setting the `brotli` option to `true` when creating a tar archive.

Outputting the results to the console or archive file
The `tar` command can output the results to the console or an archive file. To specify the output file, we can add the `-f` option followed by the output file name.

Here's an updated implementation of the `tar` command in TypeScript that includes the option to output to an archive file:

```typescript
import * as fs from 'fs';
import * as path from 'path';
import * as tar from 'tar';
import * as zlib from 'zlib';

function tarFiles(outputFile: string, files: string[], compression?: 'gzip' | 'brotli'): void {
 const tarStream = compression ?
 tar.create({ gzip: compression === 'gzip', brotli: compression === 'brotli' }) :
 tar.create();

 files.forEach(file => {
 const stat = fs.statSync(file);
 if (stat.isFile()) {
 tarStream.add(file);
 } else if (stat.isDirectory()) {
 tarStream.add(file, { recursive: true });
 }
 });

 if (outputFile) {
 const output = fs.createWriteStream(outputFile);
 const compressStream = compression ? (compression === 'gzip' ? zlib.createGzip() : zlib.createBrotliCompress()) : undefined;

 if (compressStream) {
 tarStream.pipe(compressStream).pipe(output);
 } else {
 tarStream.pipe(output);
 }

 output.on('close', () => {
 console.log(`Successfully created ${compression ? compression + ' compressed ' : ''}tar archive ${outputFile}`);
 });
 } else {
 tarStream.pipe(process.stdout);
 }
}

const outputFile = process.argv[2] === '-f' ? process.argv[3] : undefined;
const files = process.argv.slice(outputFile ? 4 : 2);

tarFiles(outputFile, files, 'gzip'); // Example: create gzip compressed tar archive
tarFiles(outputFile, files, 'brotli'); // Example: create brotli compressed tar archive
tarFiles(outputFile, files); // Example: output to console
```

This implementation takes the output file name from the command-line arguments if the `-f` option is specified. Otherwise, it outputs to the console.

The `tar.create()` method creates a tar stream that can be used to add files and directories to the archive. We loop through the list of files and add each file to the tar stream using the `tarStream.add()` method. If a directory is encountered, we use the `recursive` option to add the directory and all its contents to the archive.

If an output file is specified, we create a write stream and use the `pipe()` method to pipe the tar stream to the output stream. If the `compression` option is specified, we create a compression stream using the `zlib` module and pipe the tar stream to the compression stream before piping it to the output stream.

Finally, we listen for the `close` event on the output stream to print a success message if an output file is specified.

Testing the tar command
 However, you can manually test the `tar` command by running it on your local system with various options and inputs to verify that it works as expected.

# Chapter 12: Building a Chmod Command

Overview of the chmod command and its functionality
The `chmod` command is a utility that allows a user to modify the permissions of a file or directory. The permissions of a file or directory determine who can read, write, and execute the file or directory. There are three levels of permission: read (r), write (w), and execute (x). These permissions can be assigned to three categories of users: the owner of the file or directory, the group that the file or directory belongs to, and all other users (also known as "world").

The `chmod` command accepts a permission string as an argument, which specifies the new permissions that should be assigned to the file or directory. The permission string consists of three digits, with each digit representing the permission level for one of the three categories of users. The digits are calculated by adding up the following values:

* 4 for read permission
* 2 for write permission
* 1 for execute permission

For example, a permission string of "755" would give the owner of the file or directory read, write, and execute permission, and give the group and all other users only read and execute permission.

The `chmod` command can also be used with the symbolic notation, which allows users to modify permissions based on the existing permissions of the file or directory. The symbolic notation consists of one or more of the following operators:

* "+" adds a permission
* "-" removes a permission
* "=" sets the permission to the specified value

The symbolic notation also uses the letters "u" for the owner, "g" for the group, "o" for all other users, and "a" for all users. For example, the command "chmod u+x file.txt" would add execute permission for the owner of the file "file.txt".

Designing the Typescript implementation of the chmod command
The `chmod` command is used to modify the permissions of files or directories. In Linux and Unix-like systems, files and directories have three types of permissions: read, write, and execute. These permissions can be set for three types of users: the owner of the file or directory, users who belong to the same group as the owner, and all other users.

The `chmod` command takes a mode string as an argument, which specifies the permissions to be set. The mode string consists of a combination of numbers and letters that represent the permissions. The first character of the mode string specifies the type of the file: "-" for a regular file, "d" for a directory, "l" for a symbolic link, and so on. The next three characters represent the permissions for the owner, the next three for the group, and the last three for all other users.

The numbers 4, 2, and 1 represent read, write, and execute permissions, respectively. These numbers are added together to represent different combinations of permissions. For example, the number 7 represents read, write, and execute permissions (4 + 2 + 1), the number 6 represents read and write permissions (4 + 2), and the number 5 represents read and execute permissions (4 + 1).

Design:

For the Typescript implementation of the `chmod` command, we will need to read and process the input file or directory, parse the mode string, and apply the permissions to the file or directory. We will use the `fs` module to modify the permissions of the file or directory.

The command syntax will be:

```typescript
chmod [mode] [file/directory]
```

Where `mode` is the mode string representing the new permissions, and `file/directory` is the file or directory whose permissions will be changed.

The implementation steps will be:

1. Parse the mode string to extract the file type and the permissions for the owner, group, and other users.
2. Use the `fs.stat` method to get the current permissions of the file or directory.
3. Use the `fs.chmod` method to set the new permissions of the file or directory.

We will also need to handle errors, such as invalid input parameters or insufficient permissions to modify the file or directory.

Reading and processing the input file or directory and permission settings
The `chmod` command requires two main pieces of input - the path to the file or directory whose permissions are to be changed, and the desired permission settings.

The path to the file or directory can be provided as a command-line argument or read from user input during runtime.

The permission settings can be provided in two ways:

1. As a symbolic mode, which uses a combination of letters (`u`, `g`, `o`, `a`) and operators (`+`, `-`, `=`) to specify the desired permission settings. For example, `u+rwx` specifies that the user should be granted read, write, and execute permissions.
2. As an octal mode, which uses a three-digit number to represent the permission settings for the owner, group, and others, respectively. Each digit is the sum of the permissions for the corresponding category (read = 4, write = 2, execute = 1). For example, `755` specifies that the owner should have read, write, and execute permissions, while the group and others should have only read and execute permissions.

The input permission settings can also be modified by using the `+` and `-` operators to add or remove individual permissions, respectively.

The Typescript implementation of the `chmod` command will need to take both of these input parameters into account, as well as validate the input to ensure that the file or directory exists and that the permission settings are valid.

Implementing the chmod functionality
The `chmod` command is used to change the permissions of a file or directory. The permissions can be specified in either the symbolic or octal notation. Here's a possible implementation in TypeScript:

```typescript
import { promises as fs } from "fs";

interface ChmodOptions {
 recursive: boolean;
}

async function chmod(path: string, mode: string, options: ChmodOptions = { recursive: false }): Promise<void> {
 const stats = await fs.stat(path);

 if (options.recursive && stats.isDirectory()) {
 const entries = await fs.readdir(path);
 await Promise.all(entries.map((entry) => chmod(`${path}/${entry}`, mode, options)));
 }

 await fs.chmod(path, mode);
}
```

In this implementation, we use the `fs` module to read the current permissions of the file or directory using the `stat` function, and to modify the permissions using the `chmod` function.

The `mode` parameter can be specified in either the symbolic or octal notation. In the symbolic notation, the `mode` string consists of one or more clauses, separated by commas. Each clause consists of a scope specifier (either `u` for the user, `g` for the group, `o` for others, or `a` for all), a permission operator (`+` to add a permission, `-` to remove a permission, or `=` to set the permissions explicitly), and a permission specifier (`r` for read, `w` for write, or `x` for execute). For example, the clause `u+x` adds execute permission for the user.

In the octal notation, the `mode` string consists of a three-digit number, where each digit represents the permissions for the user, group, and others, respectively. The digits are calculated by summing the permissions: 4 for read, 2 for write, and 1 for execute. For example, the mode `755` corresponds to `rwxr-xr-x`.

The `options.recursive` parameter specifies whether to apply the permission changes recursively to all subdirectories and files. If `options.recursive` is `true` and the path is a directory, we use the `readdir` function to get a list of entries in the directory, and apply the `chmod` function recursively to each entry.

To use the `chmod` function, we can call it with the path to the file or directory, the desired mode string, and an optional `ChmodOptions` object. For example:

```typescript
await chmod("/path/to/file", "644");
await chmod("/path/to/directory", "755", { recursive: true });
```

This changes the permissions of the file to `rw-r--r--` and the permissions of the directory and its contents to `rwxr-xr-x`.

Outputting the results to the console
The `chmod` command typically does not produce any output, unless there was an error with the command execution. In that case, an error message will be displayed on the console. However, we can also display the modified file permissions after executing the `chmod` command, if the user specifies a command-line option to do so.

For example, we could add an option `-v` to the command to enable verbose output. When this option is specified, the new file permissions will be displayed for each file that was modified.

Alternatively, we could also add an option `-q` to suppress all output, including error messages, and only display output in case of successful execution. This can be useful in situations where the user wants to perform a silent `chmod` operation on a large number of files, without being prompted for confirmation or receiving any output.

Overall, the choice of output behavior for the `chmod` command depends on the user's needs and preferences.

Testing the chmod command
 However, I can suggest some scenarios that can be tested to ensure the correct behavior of the `chmod` command:

* Test changing the permissions of a file from read-only to writeable
* Test changing the permissions of a file from writeable to read-only
* Test changing the permissions of a file to be executable
* Test changing the permissions of a file to be non-executable
* Test changing the permissions of a directory to allow read, write, and execute for owner and group
* Test changing the permissions of a directory to allow only read and execute for owner and group
* Test changing the permissions of a directory to allow only write and execute for owner and group
* Test changing the permissions of a file or directory using both symbolic and absolute modes.

# Chapter 13: Building a Wget Command

Overview of the wget command and its functionality
The `wget` command is used to download files from the internet. It supports HTTP, HTTPS, and FTP protocols, and can be used to download files recursively. It also supports resuming downloads and can be used to download files in the background. The `wget` command is commonly used in shell scripts for automated downloading of files.

Designing the Typescript implementation of the wget command
The `wget` command is used to download files from the internet. Given a URL, `wget` retrieves the file and saves it to the current working directory or a specified output directory.

Here's the design of the Typescript implementation of the `wget` command:

1. Parse the input arguments to extract the URL and output directory, if specified.
2. Use the `http` or `https` module to send a GET request to the URL.
3. Pipe the response stream to a write stream to save the file to the output directory, or to the current working directory if no output directory is specified.
4. Handle errors appropriately, such as when the URL is invalid or the file cannot be saved to the output directory.

Here's an example usage of the `wget` command:

```typescript
wget https://example.com/file.txt -o /path/to/output/directory
```

This would download the file `file.txt` from `https://example.com` and save it to the directory `/path/to/output/directory`. If no output directory is specified, the file would be saved to the current working directory.

Reading and processing the input URL and download options
To implement the `wget` command in TypeScript, we need to read and process the input URL and download options.

We can take the following steps:

1. Read the input URL and download options from the command-line arguments passed to the program.
2. Parse the download options to determine the download location, filename, and whether to overwrite existing files.
3. Use the `http` or `https` module in Node.js to send a GET request to the URL.
4. Stream the response to a writable file stream, creating the file if it doesn't exist or overwriting it if the `overwrite` option is set to `true`.
5. Handle any errors that may occur during the download process, such as network errors or file system errors.

Once we have implemented these steps, we will have a functioning `wget` command that can download files from the internet.

Implementing the wget functionality
To implement the `wget` command in TypeScript, we need to read and process the input URL and download options.

We can take the following steps:

1. Read the input URL and download options from the command-line arguments passed to the program.
2. Parse the download options to determine the download location, filename, and whether to overwrite existing files.
3. Use the `http` or `https` module in Node.js to send a GET request to the URL.
4. Stream the response to a writable file stream, creating the file if it doesn't exist or overwriting it if the `overwrite` option is set to `true`.
5. Handle any errors that may occur during the download process, such as network errors or file system errors.

Once we have implemented these steps, we will have a functioning `wget` command that can download files from the internet.

Here's a TypeScript implementation of the wget command that downloads a file from a given URL and saves it to the specified file path:

```ts
import https from 'https';
import fs from 'fs';

interface DownloadOptions {
  outputPath: string;
  showProgress?: boolean;
}

function downloadFile(url: string, options: DownloadOptions): Promise<void> {
  return new Promise((resolve, reject) => {
    const { outputPath, showProgress = false } = options;
    const file = fs.createWriteStream(outputPath);

    https.get(url, response => {
      const totalBytes = parseInt(response.headers['content-length'] || '0', 10);
      let downloadedBytes = 0;

      response.on('data', chunk => {
        downloadedBytes += chunk.length;
        if (showProgress) {
          const progress = Math.floor((downloadedBytes / totalBytes) * 100);
          process.stdout.write(`Downloading... ${progress}%\r`);
        }
      });

      response.on('end', () => {
        file.end();
        resolve();
      });

      response.pipe(file);
    }).on('error', error => {
      fs.unlink(outputPath, () => {});
      reject(error);
    });
  });
}

// Example usage
downloadFile('https://example.com/image.jpg', { outputPath: './image.jpg', showProgress: true })
  .then(() => console.log('Download complete!'))
  .catch(error => console.error(`Error downloading file: ${error.message}`));
```

This implementation uses the built-in https module to download the file from the given URL and the fs module to save it to the specified file path. The downloadFile function takes two arguments: the URL to download from and an options object that includes the output file path and an optional showProgress flag that displays the download progress in the console if set to true.

The function returns a Promise that resolves when the download is complete and rejects if there is an error during the download process.

# Conclusion

There are many resources available for building command line applications in Typescript. Here are a few:

The official Typescript documentation has a section on building command line applications: <https://www.typescriptlang.org/docs/handbook/integrating-with-build-tools.html#command-line-applications>

The Commander.js library is a popular framework for building command line applications in Node.js, and has excellent Typescript support: <https://github.com/tj/commander.js#typescript>

The Yargs library is another popular option for building command line applications in Node.js, and also has Typescript support: <https://github.com/yargs/yargs#typescript>

The Inquirer.js library provides a powerful set of tools for creating interactive command line interfaces, and has Typescript support: <https://github.com/SBoudrias/Inquirer.js#typescript>

The Oclif framework provides a batteries-included toolkit for building CLI applications with TypeScript, including features like command generators and automatic help text generation: <https://oclif.io/docs/introduction>

These are just a few examples of the many resources available for building command line applications in Typescript. With its strong typing and rich tooling ecosystem, Typescript is a great choice for building robust and maintainable CLI applications.

Throughout this book, we have covered various aspects of building command-line applications using TypeScript. We started by discussing the benefits of using TypeScript for building command-line applications and set up a basic TypeScript project. We then went on to cover the most essential concepts such as parsing command-line arguments, handling file input and output, and interacting with the file system.

We then discussed the most commonly used command-line tools such as ls, df, rm, touch, locate, find, grep, tar, chmod, and wget and provided detailed information about their functionalities and how to implement them in TypeScript.

Finally, we provided additional resources for further learning and building more advanced command-line applications in TypeScript. With the knowledge gained from this book, readers should be able to build efficient and robust command-line applications in TypeScript that can handle complex tasks with ease.