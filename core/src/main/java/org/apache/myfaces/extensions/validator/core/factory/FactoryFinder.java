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
package org.apache.myfaces.extensions.validator.core.factory;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * Structure for the object that is able to locate the different factories within the ExtVal Framework.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public interface FactoryFinder
{
    /**
     * Retrieves the default or configured factory. It is instantiated the first time and retrieved from a cache in the
     * subsequent requests.
     *
     * @param factoryName The name of the factory we want to retrieve.
     * @param targetClass The class type the factory needs to be casted to.
     * @param <T> generic type parameter
     * @return Factory instance, can never be null.
     */
    <T> T getFactory(FactoryNames factoryName, Class<T> targetClass);
}
