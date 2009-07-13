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
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.context.FacesContext;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

/**
 * @author Gerhard Petracek
 * @since 1.x.3
 */
public abstract class AbstractRequestScopeAwareStorageManager<T> extends AbstractNameMapperAwareFactory<String>
        implements StorageManager<T>
{
    protected final Log logger = LogFactory.getLog(getClass());

    private List<NameMapper<String>> nameMapperList = new ArrayList<NameMapper<String>>();

    public AbstractRequestScopeAwareStorageManager()
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

    private T resolveStorage(String storageKey, String storageClassName)
    {
        Map<String, T> storageMap = resolveStorageMap();

        if(!storageMap.containsKey(storageKey))
        {
            storageMap.put(storageKey, (T)ClassUtils.tryToInstantiateClassForName(storageClassName));
        }
        return storageMap.get(storageKey);
    }

    private Map<String, T> resolveStorageMap()
    {
        Map requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();
        Map<String, T> storageMap;

        if(!requestMap.containsKey(getStorageManagerKey()))
        {
            storageMap = new HashMap<String, T>();
            requestMap.put(getStorageManagerKey(), storageMap);
        }

        return (Map<String, T>)requestMap.get(getStorageManagerKey());
    }

    public void reset(String storageKey)
    {
        Map<String, T> storageMap = resolveStorageMap();

        if(storageMap != null && storageMap.containsKey(storageKey))
        {
            storageMap.put(storageKey, null);
        }
    }

    protected List<NameMapper<String>> getNameMapperList()
    {
        return this.nameMapperList;
    }

    public abstract String getStorageManagerKey();
}