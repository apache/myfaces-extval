/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.crossval.annotation;

import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;
import java.text.DateFormat;

/**
 * @author Gerhard Petracek
 */
@Target( { METHOD, FIELD })
@Retention(RUNTIME)
@ToDo(Priority.MEDIUM, description = "DateIsEntry (value, type)")
public @interface DateIs
{
    String[] valueOf();

    /*
     * optional section
     */

    DateIsType type() default DateIsType.same;

    String validationErrorMsgKey() default "";

    String notBeforeErrorMsgKey() default "wrong_date_not_before";

    String notAfterErrorMsgKey() default "wrong_date_not_after";

    String notEqualErrorMsgKey() default "wrong_date_not_equal";

    int errorMessageDateStyle() default DateFormat.MEDIUM;
}
