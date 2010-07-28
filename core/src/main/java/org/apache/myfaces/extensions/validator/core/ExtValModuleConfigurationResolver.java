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
 * Allows the retrieval of a custom configuration object.
 *
 * @author Gerhard Petracek
 * @since r4
 */
@UsageInformation({UsageCategory.API, UsageCategory.CUSTOMIZABLE})
public interface ExtValModuleConfigurationResolver
{
    /**
     * Retrieves the custom configuration object which is of the type specified by the parameter. The type of the
     * parameter is one of the abstract classes which directly implement the ExtValModuleConfiguration interface, like
     * ExtValCoreConfiguration. The return object should not only implement the ExtValModuleConfiguration, but
     * should also extend from the class specified in the configType. The method isn't allowed to return null except
     * for the case that a custom configuration object for that type is defined as web.xml initialization parameter.
     *
     * @param configType Class type indicating the type of module for which we need to retrieve the configuration
     * @param <T>
     * @return Configuration object which ExtVal will use.
     */
    <T extends ExtValModuleConfiguration> ExtValModuleConfiguration getCustomConfiguration(Class<T> configType);
}
