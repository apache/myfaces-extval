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
package org.apache.myfaces.extensions.validator.internal;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.TYPE;
import java.lang.annotation.Target;

/**
 * it's an internal annotation to provide some information
 * <p/>
 * e.g.: some mechanisms aren't deprecated
 * they are e.g. fallbacks, alternatives,...
 * in order to avoid the deprecated annotation it's possible to use this one.
 * you can tell other developers:
 * this artifact isn't used for the desired approach, however, it's still essential to have it as ...
 * <p/>
 * idea: unify small parts of information which are frequently used
 *
 * @author Gerhard Petracek
 */
@Target({TYPE, METHOD, FIELD})
public @interface UsageInformation
{
    UsageEnum[] value();
}
