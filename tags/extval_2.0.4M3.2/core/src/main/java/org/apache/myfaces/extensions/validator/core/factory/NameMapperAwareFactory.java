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
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;

/**
 * Factory that works with NameMappers to define what should be created. The factory ask each name mapper for a possible
 * candidate for creation (name mappers can be ordered) and return the result of the first that answers with a non null
 * value.
 *
 * @author Gerhard Petracek
 * @since 1.x.2
 */
@UsageInformation(UsageCategory.API)
public interface NameMapperAwareFactory<T extends NameMapper>
{
    /**
     * Register the NameMapper for usage. When nameMapper is used in a call to the deny method, it isn't registered
     * and there is no further trace of this deny action.
     * 
     * @param classToAdd nameMapper to add.
     */
    void register(T classToAdd);

    /**
     * Removes the nameMapper from the list.
     * @param classToDeregister nameMapper to remove.
     */
    void deregister(Class<? extends NameMapper> classToDeregister);

    /**
     * Deregister the nameMapper and makes sure that the nameMapper can't be registered in the future..
     * @param classToDeny nameMapper to deny, this is, deregister and doesn't allow registration in the future.
     */
    void deny(Class<? extends NameMapper> classToDeny);
}
