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
package org.apache.myfaces.extensions.validator.core.metadata.extractor;

import java.util.logging.Logger;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.property.DefaultPropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.storage.MetaDataStorage;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ExtValAnnotationUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ProxyUtils;

/**
 * Default implementation which extracts meta-data (e.g. the annotations) of the value binding of a component.
 * It extracts the meta-data of the field and the property.
 * (Also the annotations of super classes and interfaces.)
 *
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultComponentMetaDataExtractor implements MetaDataExtractor
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    public DefaultComponentMetaDataExtractor()
    {
        logger.fine(getClass().getName() + " instantiated");
    }

    @ToDo(Priority.MEDIUM)
    public PropertyInformation extract(FacesContext facesContext, Object object)
    {
        //should never occur
        if (!(object instanceof UIComponent))
        {
            if (object != null)
            {
                this.logger.warning(object.getClass() + " is no valid component");
            }
            return new DefaultPropertyInformation();
        }

        UIComponent uiComponent = (UIComponent) object;

        logger.finest("start extracting meta-data of " + uiComponent.getClass().getName());

        PropertyDetails propertyDetails = ExtValUtils.getELHelper().getPropertyDetailsOfValueBinding(uiComponent);

        if (propertyDetails == null)
        {
            return new DefaultPropertyInformation();
        }

        /*
         * get bean class and property name
         */
        Class entityClass = ProxyUtils.getUnproxiedClass(propertyDetails.getBaseObject().getClass());

        PropertyInformation propertyInformation = getPropertyInformation(entityClass, propertyDetails);

        logger.finest("extract finished");

        return propertyInformation;
    }

    protected PropertyInformation getPropertyInformation(Class entityClass, PropertyDetails propertyDetails)
    {
        MetaDataStorage storage = getMetaDataStorage();

        PropertyInformation propertyInformation = new DefaultPropertyInformation();

        if (isCached(storage, entityClass, propertyDetails.getProperty()))
        {
            //create
            propertyInformation.setInformation(PropertyInformationKeys.PROPERTY_DETAILS, propertyDetails);

            for (MetaDataEntry metaDataEntry : getCachedMetaData(storage, entityClass, propertyDetails.getProperty()))
            {
                propertyInformation.addMetaDataEntry(metaDataEntry);
            }
        }
        else
        {
            propertyInformation = ExtValAnnotationUtils.extractAnnotations(entityClass, propertyDetails);
            cacheMetaData(storage, propertyInformation);
        }
        return propertyInformation;
    }

    protected boolean isCached(MetaDataStorage storage, Class entityClass, String property)
    {
        return storage.containsMetaDataFor(entityClass, property);
    }

    protected void cacheMetaData(MetaDataStorage storage, PropertyInformation propertyInformation)
    {
        storage.storeMetaDataOf(propertyInformation);
    }

    protected MetaDataEntry[] getCachedMetaData(MetaDataStorage storage, Class entityClass, String property)
    {
        return storage.getMetaData(entityClass, property);
    }

    protected MetaDataStorage getMetaDataStorage()
    {
        return ExtValUtils.getStorage(MetaDataStorage.class, MetaDataStorage.class.getName());
    }
}
