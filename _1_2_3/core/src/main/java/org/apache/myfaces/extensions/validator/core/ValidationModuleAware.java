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

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * if an artifact (which supports this concept) should be used just for a/some specific module(s),
 * the artifact has to implement this interface
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public interface ValidationModuleAware
{
    /**
     * during the registration process the information gets evaluated<br/>
     * instead of a class array a string array is used so that it's possible to implement
     * an artifact for different modules. if an add-on restricts an artifact to specific modules,
     * not all modules have to be used by the webapp. if a module key is unknown, the artifact won't get registered
     * for this module. if an artifact doesn't implement this interface, it gets registered for all modules
     *
     * @return an array of fully qualified class names of the module keys
     */
    String[] getModuleKeys();
}
