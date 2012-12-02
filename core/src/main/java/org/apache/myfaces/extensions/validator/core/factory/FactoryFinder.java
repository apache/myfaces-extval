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
 * Factory which is responsible to provide factories which are used by the framework.
 * An implementation should provide the possibility to use custom factory implementations.
 *
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public interface FactoryFinder
{
    /**
     * Resolves a cached factory for the given factory name.
     *
     * @param factoryName The name of the factory we want to retrieve.
     * @param targetClass The class type the factory needs to be casted to.
     * @param <T> generic type parameter
     * @return Factory instance, can never be null.
     */
    <T> T getFactory(FactoryNames factoryName, Class<T> targetClass);
}
