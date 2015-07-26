package org.jerlang.util;

import static org.assertj.core.api.StrictAssertions.assertThat;

import org.junit.Test;

public class StringUtilTest {

    @Test
    public void testSnakeToCamelCase() {
        assertThat(StringUtil.snakeToCamelCase("a_b_c")).isEqualTo("ABC");
        assertThat(StringUtil.snakeToCamelCase("ab_cd")).isEqualTo("AbCd");
        assertThat(StringUtil.snakeToCamelCase("abcd")).isEqualTo("Abcd");
        assertThat(StringUtil.snakeToCamelCase("ab")).isEqualTo("AB");
    }

    @Test
    public void testCamelToSnakeCase() {
        assertThat(StringUtil.camelToSnakeCase("AB")).isEqualTo("ab");
        assertThat(StringUtil.camelToSnakeCase("AbCd")).isEqualTo("ab_cd");
        assertThat(StringUtil.camelToSnakeCase("Abcd")).isEqualTo("abcd");
        assertThat(StringUtil.camelToSnakeCase("ABC")).isEqualTo("a_b_c");
    }

}
