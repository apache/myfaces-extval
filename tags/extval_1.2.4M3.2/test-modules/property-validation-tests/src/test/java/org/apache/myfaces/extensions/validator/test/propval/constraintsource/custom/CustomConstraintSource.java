package org.apache.myfaces.extensions.validator.test.propval.constraintsource.custom;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

@Target({TYPE, FIELD, METHOD})
@Retention(RUNTIME)
@Documented
public @interface CustomConstraintSource
{
    public abstract Class value();
}
