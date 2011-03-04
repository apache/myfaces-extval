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

/**
 * Interface for name-mappers which have to delegate to other (sub-)name-mappers.
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public interface SubMapperAwareNameMapper<T> extends NameMapper<T>
{
    /**
     * Add a (sub-)name mapper
     * @param nameMapper sub-nameMapper to add
     */
    void addNameMapper(NameMapper<T> nameMapper);

    /**
     * Deregisters all (sub-)name mappers of the given type
     * @param nameMapperClass type of the (sub-)name mappers which have to be deregistered
     */
    void removeNameMapper(Class<? extends NameMapper> nameMapperClass);
}