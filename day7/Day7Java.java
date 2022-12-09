package day7;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day7Java {
    public static void main(String[] args) throws IOException {
        List<String> rawLines = Files.readAllLines(Path.of("day7/day7.txt"));
        Stack<Directory> directoryStack = new Stack<>();
        Directory root = new Directory("/");

        Directory currentDir = root;
        Pattern fileLinePattern = Pattern.compile("(\\d+)\\s(.*)");
        rawLines.remove(0);
        for (String line : rawLines) {
            if (line.equals("$ cd ..")) {
                currentDir = directoryStack.pop();
                continue;
            }
            if (line.startsWith("$ cd")) {
                String nextDirName = line.replace("$ cd", "").trim();
                Directory nextDirectory = currentDir.getDir(nextDirName);
                currentDir.add(nextDirectory);
                directoryStack.push(currentDir); //add the current folder into the stack to keep track of for `cd ..` operations
                currentDir = nextDirectory;
                continue;
            }
            if (line.startsWith("dir")) {
                String dirName = line.replace("dir", "").trim();
                Directory d = new Directory(dirName);
                currentDir.add(d);
                continue;
            }
            if (line.equals("$ ls")) {
                continue; //I just don't care
            }

            //must be a file
            Matcher matcher = fileLinePattern.matcher(line);
            matcher.matches();
            String stringFileSize = matcher.group(1);
            String fileName = matcher.group(2);
            File newFile = new File(fileName, Long.parseLong(stringFileSize));
            currentDir.add(newFile);
        }


        Collection<Directory> allDirs = root.getAllDirs();
        long part1 = allDirs.stream().map(dir -> dir.size()).filter(size -> size <= 100000).reduce(Long::sum).orElseGet(() -> 0l);
        System.out.println("Part 1: " + part1);

        final long totalSize = 70000000;
        final long spaceRequired = 30000000;
        final long currentlyUsed = root.size();


        long part2 = allDirs.stream().filter(dir -> {
            return (totalSize - currentlyUsed + dir.size()) > spaceRequired;
        }).sorted(Comparator.comparingLong(Directory::size)).findFirst().get().size();

        System.out.println("Part 2: " + part2);

    }

    static class Directory {

        private final String name;
        private final Map<String, Directory> subDirectories = new HashMap<>();
        private final Set<File> files = new HashSet<>();
        private long totalSize = -1;

        public Directory(String name) {
            this.name = name;
        }


        public long size() {
            if(totalSize > 0){
                return totalSize;
            }
            long fileSize = files.stream().map(f -> f.size()).reduce(Long::sum).orElseGet(() -> 0l);
            long subDirsSize = subDirectories.values().stream().map(f -> f.size()).reduce(Long::sum).orElseGet(() -> 0l);
            totalSize = fileSize + subDirsSize;
            return totalSize;
        }

        public String name() {
            return name;
        }

        public void add(Directory newDir) {
            subDirectories.put(newDir.name(), newDir);
        }

        public void add(File f) {
            files.add(f);
        }

        @Override
        public String toString() {
            return "Directory{" +
                    "name='" + name + '\'' +
                    ", files=" + files +
                    ", subDirectories=" + subDirectories +
                    '}';
        }

        public Directory getDir(String nextDirName) {
            return subDirectories.get(nextDirName);
        }

        public Collection<Directory> getAllDirs() {
            List<Directory> flattenedSubDirs = new ArrayList<>();
            flattenedSubDirs.addAll(subDirectories.values());
            subDirectories.values().forEach(subDir -> {
                flattenedSubDirs.addAll(subDir.getAllDirs());
            });
            return flattenedSubDirs;
        }
    }

    record File(String name, long size) {
        @Override
        public String toString() {
            return "File{" +
                    "name='" + name + '\'' +
                    ", size=" + size +
                    '}';
        }
    }
}
