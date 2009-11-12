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
package org.apache.myfaces.extensions.validator.core;

import java.lang.annotation.Target;
import java.lang.annotation.Retention;
import java.lang.annotation.Documented;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import static java.lang.annotation.ElementType.TYPE;

@Target(TYPE)
@Retention(RUNTIME)
@Documented
/**
 * allowed to use for classes which implement interfaces which have the marker @InvocationOrderSupport
 *
 * suggested ranges (mainly for name-mappers):
 * negative values for "extreme" cases
 *
 * 0-49 for custom artifacts which should have the highest priority
 * 50-99 for add-ons which provide artifacts which should have a higher priority than the default artifacts
 * 100-999 internal artifacts
 * 1000+ for custom artifacts
 *
 * suggested ranges for artifacts like interceptors,...
 * 1xx ... artifacts of the core
 * 2xx ... artifacts of validation modules
 * 3xx ... artifacts of component support modules
 *
 * a priority should be unique within one artifact-type - that means
 * if a name-mapper has priority 100, it's ok that an exception-interceptor also has priority 100.
 * but a 2nd name-mapper shouldn't have priority 100
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
public @interface InvocationOrder
{
    /**
     * default priority for custom artifacts (if they should get added after the internal versions
     * @return the priority of an artifact annotated with this annotation
     */
    int value() default 1000;
}