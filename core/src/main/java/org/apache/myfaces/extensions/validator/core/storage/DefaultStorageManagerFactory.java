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

import org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.factory.AbstractNameMapperAwareFactory;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationNames;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationEntry;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import static org.apache.myfaces.extensions.validator.internal.UsageCategory.INTERNAL;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

/**
 * default implementation for storage-manager creation and caching
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(INTERNAL)
public class DefaultStorageManagerFactory extends AbstractNameMapperAwareFactory<Class>
        implements ClassMappingFactory<Class, StorageManager>, StorageManagerHolder
{
    protected final Log logger = LogFactory.getLog(getClass());

    private boolean lazyStaticMappingApplied = false;
    private List<NameMapper<Class>> nameMapperList = new ArrayList<NameMapper<Class>>();
    private Map<Class, StorageManager> storageTypeToStorageManagerMap = new HashMap<Class, StorageManager>();

    public DefaultStorageManagerFactory()
    {
        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }

        setStorageManager(RendererProxyStorage.class,
                new DefaultRendererProxyStorageManager(), false);
        setStorageManager(GroupStorage.class,
                new DefaultGroupStorageManager(), false);
        setStorageManager(MetaDataStorage.class,
                new DefaultMetaDataStorageManager(), false);
        setStorageManager(FacesMessageStorage.class,
                new DefaultFacesMessageStorageManager(), false);
        setStorageManager(PropertyStorage.class,
                new DefaultPropertyStorageManager(), false);
        setStorageManager(RendererInterceptorPropertyStorage.class,
                new DefaultRendererInterceptorPropertyStorageManager(), false);
        setStorageManager(ViolationSeverityInterpreterStorage.class,
                new DefaultViolationSeverityInterpreterStorageManager(), false);

        setStorageManager(
                FacesInformationStorage.class, new DefaultFacesInformationStorageManager(), false);
    }

    public StorageManager create(Class storageType)
    {
        if (!this.lazyStaticMappingApplied)
        {
            initStaticMappings();
        }

        StorageManager storageManager;
        String storageManagerName;
        //null -> use name mappers
        for (NameMapper<Class> nameMapper : this.nameMapperList)
        {
            storageManagerName = nameMapper.createName(storageType);

            if (storageManagerName == null)
            {
                continue;
            }

            storageManager = (StorageManager)ClassUtils.tryToInstantiateClassForName(storageManagerName);

            if (storageManager != null)
            {
                addMapping(storageType, storageManager);
                return storageManager;
            }
        }
        return getStorageManager(storageType);
    }

    private synchronized void addMapping(Class storageType, StorageManager storageManager)
    {
        boolean isValidEntry = true;
        if(storageType == null)
        {
            isValidEntry = false;
            if(this.logger.isErrorEnabled())
            {
                this.logger.error("you tried to add an invalid storage type");
            }
        }

        if(storageManager == null)
        {
            isValidEntry = false;
            if(this.logger.isErrorEnabled())
            {
                this.logger.error("you tried to add an invalid storage manager");
            }
        }

        if(!isValidEntry)
        {
            return;
        }

        setStorageManager(storageType, storageManager, true);
    }

    private void initStaticMappings()
    {
        this.lazyStaticMappingApplied = true;

        //setup internal static mappings
        for (StaticConfiguration<String, String> staticConfig :
            ExtValContext.getContext().getStaticConfiguration(
                StaticConfigurationNames.STORAGE_TYPE_TO_STORAGE_MANAGER_CONFIG))
        {
            setupMappings(staticConfig.getMapping());
        }
    }

    private void setupMappings(List<StaticConfigurationEntry<String, String>> mappings)
    {
        for(StaticConfigurationEntry<String, String> mapping : mappings)
        {
            addMapping(ClassUtils.tryToLoadClassForName(mapping.getSource()),
                    (StorageManager)ClassUtils.tryToInstantiateClassForName(mapping.getTarget()));
        }
    }

    protected List<NameMapper<Class>> getNameMapperList()
    {
        return this.nameMapperList;
    }

    public void setStorageManager(Class storageType, StorageManager storageManager, boolean override)
    {
        if(!this.storageTypeToStorageManagerMap.containsKey(storageType) ||
                (this.storageTypeToStorageManagerMap.containsKey(storageType) && override))
        {

            if(logger.isTraceEnabled())
            {
                logger.trace("adding type to storage-manager mapping: "
                    + storageType.getName() + " -> " + storageManager.getClass().getName());
            }

            this.storageTypeToStorageManagerMap.put(storageType, storageManager);
        }
    }

    public StorageManager getStorageManager(Class type)
    {
        return this.storageTypeToStorageManagerMap.get(type);
    }
}
