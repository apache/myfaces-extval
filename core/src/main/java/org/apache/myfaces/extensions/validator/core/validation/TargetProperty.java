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
package org.apache.myfaces.extensions.validator.core.validation;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import java.lang.annotation.Target;
import java.lang.annotation.Retention;
import java.lang.annotation.Documented;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * To specify which property should be used as constraint source (instead of the default naming convention).
 *
 * If needed, this annotation can be replaced by a custom annotation to keep the implementation independent of ExtVal.
 * (see targetPropertyAnnotation in ExtValCoreConfiguration)
 *
 * @see org.apache.myfaces.extensions.validator.core.validation.ConstraintSource
 *
 * @since r4
 */
@Target({FIELD, METHOD})
@Retention(RUNTIME)
@Documented
@UsageInformation(UsageCategory.API)
public @interface TargetProperty
{
    /**
     * property-name available at the target which should be used as constraint source
     *
     * @return property-name which has to exist at the target-cass
     */
    String value();
}
