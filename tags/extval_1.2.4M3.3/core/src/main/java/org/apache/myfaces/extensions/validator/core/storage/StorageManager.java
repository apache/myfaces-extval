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
package org.apache.myfaces.extensions.validator.core.storage;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import static org.apache.myfaces.extensions.validator.internal.UsageCategory.API;

/**
 * Storage managers are responsible to create and reset specific storage implementations depending on the scope.
 * A manager can be responsible for multiple (named) storages.
 *
 * @param <T> Type of Storage that is maintained by the storageManager.
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(API)
public interface StorageManager<T>
{
    /**
     * Creates and scope or retrieve a previously created instance, of the storage for the given key.
     *
     * @param key The key for a storage that needs to be created
     * @return The storage associated with the key or null if the key is unknown to the storageManager.
     */
    T create(String key);

    /**
     * Resets the storage linked to the given key.
     * If the storage isn't created yet or the key is unknown
     * for the storageManager, nothing is performed and no exception is thrown.
     * 
     * @param key The key for a storage that needs to be resetted
     */
    void reset(String key);
}