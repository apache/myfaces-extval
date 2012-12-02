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
 * @since r4
 */
@UsageInformation({UsageCategory.API, UsageCategory.CUSTOMIZABLE})
public interface ExtValModuleConfigurationResolver
{
    /**
     * Retrieves the custom configuration which matches the given config-type.
     * The type of the parameter is one of the abstract classes which directly implements the
     * {@link ExtValModuleConfiguration} interface, like {@link ExtValCoreConfiguration}.
     * The returned config has to extend one of the abstract config classes.
     *
     * (The method isn't allowed to return null except
     * for the case that a custom configuration for that type is defined via a web.xml context-param.)
     *
     * @param configType Class that specifies the type of the target config.
     * @param <T>
     * @return Configuration object which will be used by ExtVal.
     */
    <T extends ExtValModuleConfiguration> ExtValModuleConfiguration getCustomConfiguration(Class<T> configType);
}
