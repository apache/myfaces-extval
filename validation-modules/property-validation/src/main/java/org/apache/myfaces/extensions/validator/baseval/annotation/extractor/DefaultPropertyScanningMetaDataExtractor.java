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
package org.apache.myfaces.extensions.validator.baseval.annotation.extractor;

import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.property.DefaultPropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.DefaultComponentMetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.storage.PropertyStorage;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ProxyUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import static org.apache.myfaces.extensions.validator.util.ExtValAnnotationUtils.addFieldAccessAnnotations;
import static org.apache.myfaces.extensions.validator.util.ExtValAnnotationUtils.addPropertyAccessAnnotations;
import org.apache.myfaces.extensions.validator.PropertyValidationModuleKey;

import javax.faces.context.FacesContext;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@Deprecated
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultPropertyScanningMetaDataExtractor extends DefaultComponentMetaDataExtractor
{
    private DefaultPropertyScanningMetaDataExtractor()
    {
    }

    public static MetaDataExtractor getInstance()
    {
        return ExtValUtils.createInterceptedMetaDataExtractorFor(
                new DefaultPropertyScanningMetaDataExtractor(), PropertyValidationModuleKey.class);
    }

    @Override
    @ToDo(Priority.MEDIUM)
    public PropertyInformation extract(FacesContext facesContext, Object object)
    {
        PropertyInformation propertyInformation = new DefaultPropertyInformation();

        if (!(object instanceof PropertyDetails))
        {
            throw new IllegalStateException(object.getClass() + " is not a " + PropertyDetails.class.getName());
        }

        PropertyDetails propertyDetails = (PropertyDetails)object;

        Class entityClass = ProxyUtils.getUnproxiedClass(propertyDetails.getBaseObject().getClass());

        //TODO test with complex components
        propertyInformation.setInformation(
            PropertyInformationKeys.PROPERTY_DETAILS, propertyDetails);

        /*
         * find and add annotations
         */
        PropertyStorage storage = ReflectionUtils.getPropertyStorage();

        addPropertyAccessAnnotations(storage, entityClass, propertyDetails.getProperty(), propertyInformation);
        addFieldAccessAnnotations(storage, entityClass, propertyDetails.getProperty(), propertyInformation);

        return propertyInformation;
    }
}
