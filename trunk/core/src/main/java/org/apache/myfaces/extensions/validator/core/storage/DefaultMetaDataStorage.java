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
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import static org.apache.myfaces.extensions.validator.internal.UsageCategory.INTERNAL;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.property.DefaultPropertyInformation;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.CustomInformation;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(INTERNAL)
public class DefaultMetaDataStorage implements MetaDataStorage
{
    protected final Log logger = LogFactory.getLog(getClass());

    private Map<String, PropertyInformation> cachedPropertyInformation = new HashMap<String, PropertyInformation>();

    private List<MetaDataStorageFilter> metaDataStorageFilters = new ArrayList<MetaDataStorageFilter>();
    private List<Class<? extends MetaDataStorageFilter>> deniedMetaDataFilters =
            new ArrayList<Class<? extends MetaDataStorageFilter>>();

    public DefaultMetaDataStorage()
    {
        initFilters();
    }

    private void initFilters()
    {
        List<String> metaDataStorageFilterClassNames = new ArrayList<String>();

        metaDataStorageFilterClassNames
            .add(WebXmlParameter.CUSTOM_META_DATA_STORAGE_FILTER);
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

        this.cachedPropertyInformation.put(
                createKey(propertyDetails.getBaseObject().getClass(), propertyDetails.getProperty()),
                propertyInformationToStore);
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
        PropertyInformation propertyInformation = this.cachedPropertyInformation
                .get(createKey(targetClass, targetProperty));

        PropertyInformation clonedPropertyInformation = new DefaultPropertyInformation();
        copyMetaData(propertyInformation, clonedPropertyInformation);

        return clonedPropertyInformation.getMetaDataEntries();
    }

    public boolean containsMetaDataFor(Class targetClass, String targetProperty)
    {
        return this.cachedPropertyInformation.containsKey(createKey(targetClass, targetProperty));
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
        return this.deniedMetaDataFilters.contains(storageFilter.getClass());
    }

    private boolean isFilterAlreadyRegistered(MetaDataStorageFilter storageFilter)
    {
        for(MetaDataStorageFilter filter : this.metaDataStorageFilters)
        {
            if(filter.getClass().equals(storageFilter.getClass()))
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

    private String createKey(Class targetClass, String targetProperty)
    {
        String targetClassName = getTargetClassName(targetClass);
        return targetClassName + "#" + targetProperty;
    }

    private String getTargetClassName(Class currentClass)
    {
        if (currentClass.getName().contains("$$EnhancerByCGLIB$$")
            || currentClass.getName().contains("$$FastClassByCGLIB$$"))
        {
            return currentClass.getName().substring(0, currentClass.getName().indexOf("$"));
        }
        return currentClass.getName();
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
        if(this.logger.isInfoEnabled())
        {
            this.logger.info(filterClass.getName() + " added");
        }
    }

    private void logRemovedFilter(Class<? extends MetaDataStorageFilter> filterClass)
    {
        if(this.logger.isInfoEnabled())
        {
            this.logger.info(filterClass.getName() + " removed");
        }
    }
}
