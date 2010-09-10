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

import org.apache.myfaces.extensions.validator.core.factory.AbstractNameMapperAwareFactory;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.util.ProxyUtils;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import static org.apache.myfaces.extensions.validator.internal.UsageCategory.REUSE;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.logging.Logger;

/**
 * An implementation of {@link AbstractStorageManager} is responsible to
 * manage storages ({@see AbstractStorageManager#create} and {@see AbstractStorageManager#reset})
 * in the context of a specific scope (e.g. the application scope of JSF).
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(REUSE)
public abstract class AbstractStorageManager<T> extends AbstractNameMapperAwareFactory<String>
        implements StorageManager<T>
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    private List<NameMapper<String>> nameMapperList = new CopyOnWriteArrayList<NameMapper<String>>();

    public AbstractStorageManager()
    {
        logger.fine(getClass().getName() + " instantiated");
    }

    /**
     * {@inheritDoc}
     * A storage is resolved via the registered name mappers.
     */
    public T create(String storageName)
    {
        T storageManager;
        String storageClassName;
        //null -> use name mappers
        for (NameMapper<String> nameMapper : this.nameMapperList)
        {
            storageClassName = nameMapper.createName(storageName);

            if (storageClassName == null)
            {
                continue;
            }

            storageManager = resolveStorage(storageName, storageClassName);

            if (storageManager != null)
            {
                return storageManager;
            }
        }
        return null;
    }

    /**
     * Retrieves the storage Manager from the cached instances, if already created previously. Otherwise
     * a new instance is created and stored in the cache.
     *
     * @param storageKey  The type of storage that needs to be created
     * @param storageClassName  The class name of the implementation of the storage manager.
     * @return storage instance for the given parameters or null in case of invalid parameters.
     */
    protected T resolveStorage(String storageKey, String storageClassName)
    {
        Map<String, T> storageMap = resolveStorageMap();

        if(!storageMap.containsKey(storageKey))
        {
            synchronized (storageMap)
            {
                storageMap.put(storageKey, (T)ClassUtils.tryToInstantiateClassForName(storageClassName));
            }
        }
        return storageMap.get(storageKey);
    }

    /**
     * Returns a Map of all scoped storages stored by the current manager instance.
     *
     * @return Map of all cached storages managed by the current storage manager.
     */
    protected abstract Map<String, T> resolveStorageMap();

    public synchronized void reset(String storageKey)
    {
        Map<String, T> storageMap = resolveStorageMap();

        if(storageMap != null && storageMap.containsKey(storageKey))
        {
            Class storageClass = ProxyUtils.getUnproxiedClass(storageMap.get(storageKey).getClass());
            storageMap.put(storageKey, (T)ClassUtils.tryToInstantiateClass(storageClass));
        }
    }

    protected List<NameMapper<String>> getNameMapperList()
    {
        return this.nameMapperList;
    }

    /**
     * Returns the key which will be used to find the storages scoped by the storage-manager instance.
     *
     * @return key for finding the storage scoped by the storage-manager instance
     */
    public abstract String getStorageManagerKey();
}
