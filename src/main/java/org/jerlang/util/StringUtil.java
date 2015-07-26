package org.jerlang.util;

public class StringUtil {

    /**
     * Converts a string from snake_case to CamelCase.
     */
    public static String snakeToCamelCase(String string) {
        if (string.length() <= 2) {
            return string.toUpperCase();
        }
        StringBuilder result = new StringBuilder(string.length());
        boolean capitalize = true;
        for (char c : string.toCharArray()) {
            if (c == '_') {
                capitalize = true;
            } else if (capitalize) {
                result.append(Character.toUpperCase(c));
                capitalize = false;
            } else {
                result.append(c);
            }
        }
        return result.toString();
    }

    /**
     * Converts a string from CamelCase to snake_case.
     */
    public static String camelToSnakeCase(String string) {
        if (string.length() <= 2) {
            return string.toLowerCase();
        }
        StringBuilder result = new StringBuilder(string.length());
        for (char c : string.toCharArray()) {
            if (Character.isUpperCase(c)) {
                if (result.length() > 0) {
                    result.append('_');
                }
                result.append(Character.toLowerCase(c));
            } else {
                result.append(c);
            }
        }
        return result.toString();
    }

}
