package org.jerlang.util;

import static org.assertj.core.api.StrictAssertions.assertThat;
import static org.jerlang.util.StringUtil.camelToSnakeCase;
import static org.jerlang.util.StringUtil.snakeToCamelCase;

import org.junit.Test;

public class StringUtilTest {

    @Test
    public void testSnakeToCamelCase() {
        assertThat(snakeToCamelCase("aa_bb_cc")).isEqualTo("AaBbCc");
        assertThat(snakeToCamelCase("ab_cd")).isEqualTo("AbCd");
        assertThat(snakeToCamelCase("abcd")).isEqualTo("Abcd");
        // Exceptions
        assertThat(snakeToCamelCase("abs")).isEqualTo("Abs");
        assertThat(snakeToCamelCase("os")).isEqualTo("OS");
        assertThat(snakeToCamelCase("md5")).isEqualTo("MD5");
    }

    @Test
    public void testCamelToSnakeCase() {
        assertThat(camelToSnakeCase("AaBbCc")).isEqualTo("aa_bb_cc");
        assertThat(camelToSnakeCase("AbCd")).isEqualTo("ab_cd");
        assertThat(camelToSnakeCase("Abcd")).isEqualTo("abcd");
        // Exceptions
        assertThat(camelToSnakeCase("Abs")).isEqualTo("abs");
        assertThat(camelToSnakeCase("OS")).isEqualTo("os");
        assertThat(camelToSnakeCase("MD5")).isEqualTo("md5");
    }

}
