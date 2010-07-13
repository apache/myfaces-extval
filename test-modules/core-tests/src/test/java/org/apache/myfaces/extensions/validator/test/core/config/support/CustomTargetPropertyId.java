package org.apache.myfaces.extensions.validator.test.core.config.support;

import java.lang.annotation.Annotation;
import java.lang.annotation.Documented;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;

@Target({FIELD, METHOD})
@Retention(RUNTIME)
@Documented
public @interface CustomTargetPropertyId
{
    Class<? extends Annotation> value();
}
