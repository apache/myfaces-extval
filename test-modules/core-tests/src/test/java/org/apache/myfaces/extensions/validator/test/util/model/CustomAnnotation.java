package org.apache.myfaces.extensions.validator.test.util.model;

import java.lang.annotation.Documented;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;

@Target({METHOD, FIELD})
@Retention(RUNTIME)
@Documented
public @interface CustomAnnotation
{
    boolean required();
}
