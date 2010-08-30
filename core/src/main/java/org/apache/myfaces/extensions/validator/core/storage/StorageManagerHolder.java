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
 * Interface to manage storage-manager instances.
 *
 * @see org.apache.myfaces.extensions.validator.core.storage.StorageManager
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(API)
public interface StorageManagerHolder
{
    /**
     * Define the storage manager for the specified type.
     * @param type The identification for the storage manager, by convention it is the object type managed by the
     * storageManager.
     * @param storageManager The storageManager to use.
     * @param override if true, the previous defined storageManager is no longer used.  If false, the specified
     * storage manager is only used when no manager was defined.
     */
    void setStorageManager(Class type, StorageManager storageManager, boolean override);

    /**
     * Retrieves the storageManager for the specified type.
     * @param type The identification for the storage manager, by convention it is the object type managed by the
     * storageManager.
     * @return the storageManager specified for the type or null if no manager specified for the type.
     */
    StorageManager getStorageManager(Class type);
}