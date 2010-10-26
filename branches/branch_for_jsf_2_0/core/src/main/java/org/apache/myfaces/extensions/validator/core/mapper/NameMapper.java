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
package org.apache.myfaces.extensions.validator.core.mapper;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.core.InvocationOrderSupport;

/**
 * Interface for name mappers.
 * A name mapper calculates a name of the resource which is linked to the given source.
 * e.g. constraints are mapped to the corresponding
 * {@link org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy}.
 * NameMappers have to be stateless.
 * 
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@InvocationOrderSupport
@UsageInformation(UsageCategory.API)
public interface NameMapper<T>
{
    /**
     * Returns the name of the target resource for the given source or null if the implementations isn't responsible
     * to map the given instance.
     *
     * @param source Object for which the name must be created.
     * @return name of the target resource or null if the source isn't supported.
     */
    String createName(T source);
}