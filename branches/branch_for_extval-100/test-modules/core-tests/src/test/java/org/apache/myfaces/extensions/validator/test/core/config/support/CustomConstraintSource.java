package org.apache.myfaces.extensions.validator.test.core.config.support;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import java.lang.annotation.Documented;
import static java.lang.annotation.ElementType.*;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;

@Target({TYPE, FIELD, METHOD})
@Retention(RUNTIME)
@Documented
@UsageInformation(UsageCategory.API)
public @interface CustomConstraintSource
{
    Class value();
}
