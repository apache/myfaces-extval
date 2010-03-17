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
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;

/**
 * generic storage manager implementation
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(REUSE)
public abstract class AbstractStorageManager<T> extends AbstractNameMapperAwareFactory<String>
        implements StorageManager<T>
{
    protected final Log logger = LogFactory.getLog(getClass());

    private List<NameMapper<String>> nameMapperList = new ArrayList<NameMapper<String>>();

    public AbstractStorageManager()
    {
        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

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

    protected T resolveStorage(String storageKey, String storageClassName)
    {
        Map<String, T> storageMap = resolveStorageMap();

        if(!storageMap.containsKey(storageKey))
        {
            storageMap.put(storageKey, (T)ClassUtils.tryToInstantiateClassForName(storageClassName));
        }
        return storageMap.get(storageKey);
    }

    protected abstract Map<String, T> resolveStorageMap();

    public void reset(String storageKey)
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

    public abstract String getStorageManagerKey();
}
