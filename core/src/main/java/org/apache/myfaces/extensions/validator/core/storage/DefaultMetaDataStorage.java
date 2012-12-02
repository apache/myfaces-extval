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

import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import static org.apache.myfaces.extensions.validator.internal.UsageCategory.INTERNAL;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.property.DefaultPropertyInformation;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.CustomInformation;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.util.ProxyUtils;
import org.apache.myfaces.extensions.validator.util.NullValueAwareConcurrentHashMap;

import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.logging.Logger;

/**
 * @since x.x.3
 */
@UsageInformation(INTERNAL)
public class DefaultMetaDataStorage implements MetaDataStorage
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    private Map<String, Map<String, PropertyInformation>> cachedPropertyInformation =
            new ConcurrentHashMap<String, Map<String, PropertyInformation>>();

    private List<MetaDataStorageFilter> metaDataStorageFilters = new CopyOnWriteArrayList<MetaDataStorageFilter>();
    private List<Class<? extends MetaDataStorageFilter>> deniedMetaDataFilters =
            new CopyOnWriteArrayList<Class<? extends MetaDataStorageFilter>>();

    public DefaultMetaDataStorage()
    {
        initFilters();
    }

    private void initFilters()
    {
        List<String> metaDataStorageFilterClassNames = new ArrayList<String>();

        metaDataStorageFilterClassNames
            .add(ExtValCoreConfiguration.get().customMetaDataStorageFilterClassName());
        metaDataStorageFilterClassNames
            .add(ExtValContext.getContext().getInformationProviderBean().get(
                    CustomInformation.META_DATA_STORAGE_FILTER));

        MetaDataStorageFilter metaDataStorageFilter;
        for (String validationExceptionInterceptorName : metaDataStorageFilterClassNames)
        {
            metaDataStorageFilter =
                (MetaDataStorageFilter)ClassUtils.tryToInstantiateClassForName(validationExceptionInterceptorName);

            if (metaDataStorageFilter != null)
            {
                this.metaDataStorageFilters.add(metaDataStorageFilter);

                logAddedFilter(metaDataStorageFilter.getClass());
            }
        }
    }

    public void storeMetaDataOf(PropertyInformation propertyInformation)
    {
        invokeFilters(propertyInformation);

        PropertyInformation propertyInformationToStore = new DefaultPropertyInformation();

        PropertyDetails propertyDetails = propertyInformation
                .getInformation(PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class);

        copyMetaData(propertyInformation, propertyInformationToStore);

        getMapForClass(ProxyUtils.getUnproxiedClass(propertyDetails.getBaseObject().getClass()))
                .put(propertyDetails.getProperty(), propertyInformationToStore);
    }

    private void invokeFilters(PropertyInformation propertyInformation)
    {
        for(MetaDataStorageFilter filter : this.metaDataStorageFilters)
        {
            filter.filter(propertyInformation);
        }
    }

    public MetaDataEntry[] getMetaData(Class targetClass, String targetProperty)
    {
        PropertyInformation propertyInformation = getMapForClass(targetClass).get(targetProperty);

        PropertyInformation clonedPropertyInformation = new DefaultPropertyInformation();
        copyMetaData(propertyInformation, clonedPropertyInformation);

        return clonedPropertyInformation.getMetaDataEntries();
    }

    public boolean containsMetaDataFor(Class targetClass, String targetProperty)
    {
        return getMapForClass(targetClass).containsKey(targetProperty);
    }

    public void registerFilter(MetaDataStorageFilter storageFilter)
    {
        synchronized (this)
        {
            if(!isFilterDenied(storageFilter) && !isFilterAlreadyRegistered(storageFilter))
            {
                this.metaDataStorageFilters.add(storageFilter);
                logAddedFilter(storageFilter.getClass());
            }
        }
    }

    private boolean isFilterDenied(MetaDataStorageFilter storageFilter)
    {
        return this.deniedMetaDataFilters.contains(getStorageFilterClass(storageFilter));
    }

    private boolean isFilterAlreadyRegistered(MetaDataStorageFilter storageFilter)
    {
        for(MetaDataStorageFilter filter : this.metaDataStorageFilters)
        {
            if(filter.getClass().equals(getStorageFilterClass(storageFilter)))
            {
                return true;
            }
        }
        return false;
    }

    public void deregisterFilter(Class<? extends MetaDataStorageFilter> filterClass)
    {
        MetaDataStorageFilter storageFilter = ClassUtils.tryToInstantiateClass(filterClass);

        synchronized (this)
        {
            this.metaDataStorageFilters.remove(storageFilter);
        }

        logRemovedFilter(storageFilter.getClass());
    }

    public void denyFilter(Class<? extends MetaDataStorageFilter> filterClass)
    {
        synchronized (this)
        {
            for(Class<? extends MetaDataStorageFilter> filterId : this.deniedMetaDataFilters)
            {
                if(filterId.equals(filterClass))
                {
                    return;
                }
            }
            this.deniedMetaDataFilters.add(filterClass);
        }

        deregisterFilter(filterClass);
    }

    @ToDo(Priority.MEDIUM)
    private void copyMetaData(PropertyInformation source, PropertyInformation target)
    {
        MetaDataEntry newMetaDataEntry;
        for(MetaDataEntry metaDataEntry : source.getMetaDataEntries())
        {
            newMetaDataEntry = new MetaDataEntry();
            newMetaDataEntry.setKey(metaDataEntry.getKey());
            newMetaDataEntry.setValue(metaDataEntry.getValue());

            target.addMetaDataEntry(newMetaDataEntry);
        }
    }

    private void logAddedFilter(Class<? extends MetaDataStorageFilter> filterClass)
    {
        this.logger.info(filterClass.getName() + " added");
    }

    private void logRemovedFilter(Class<? extends MetaDataStorageFilter> filterClass)
    {
        this.logger.info(filterClass.getName() + " removed");
    }

    private Map<String, PropertyInformation> getMapForClass(Class target)
    {
        String key = ProxyUtils.getClassName(target);
        if(!this.cachedPropertyInformation.containsKey(key))
        {
            this.cachedPropertyInformation.put(key,
                new NullValueAwareConcurrentHashMap<String, PropertyInformation>(new NullMarkerPropertyInformation()));
        }
        return this.cachedPropertyInformation.get(key);
    }

    private static class NullMarkerPropertyInformation implements PropertyInformation
    {
        public boolean containsInformation(String key)
        {
            throw new UnsupportedOperationException();
        }

        public Object getInformation(String key)
        {
            throw new UnsupportedOperationException();
        }

        public <T> T getInformation(String key, Class<T> targetClass)
        {
            throw new UnsupportedOperationException();
        }

        public void setInformation(String key, Object value)
        {
            throw new UnsupportedOperationException();
        }

        public MetaDataEntry[] getMetaDataEntries()
        {
            throw new UnsupportedOperationException();
        }

        public void addMetaDataEntry(MetaDataEntry metaDataEntry)
        {
            throw new UnsupportedOperationException();
        }

        public void resetMetaDataEntries()
        {
            throw new UnsupportedOperationException();
        }

        @Override
        public int hashCode()
        {
            return getClass().hashCode();
        }

        @Override
        public boolean equals(Object target)
        {
            return target != null && getClass().equals(target.getClass());
        }
    }

    private Class<? extends MetaDataStorageFilter> getStorageFilterClass(MetaDataStorageFilter storageFilter)
    {
        return ProxyUtils.getUnproxiedClass(storageFilter.getClass(), MetaDataStorageFilter.class);
    }
}
