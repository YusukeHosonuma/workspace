package language.property.client;

import language.property.FileSystemConstants;

public class TopLevelPropertyClient {
    public static void main(String[] args) {

        System.out.println(FileSystemConstants.getUnixLineSeparator());
        System.out.println(FileSystemConstants.WINDOWS_LINE_SEPARATOR);
    }
}
