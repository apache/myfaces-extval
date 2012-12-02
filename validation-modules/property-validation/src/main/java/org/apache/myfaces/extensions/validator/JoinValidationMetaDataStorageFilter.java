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
package org.apache.myfaces.extensions.validator;

import org.apache.myfaces.extensions.validator.baseval.annotation.JoinValidation;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.DefaultComponentMetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.property.DefaultPropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.storage.MetaDataStorageFilter;
import org.apache.myfaces.extensions.validator.core.storage.PropertyStorage;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ProxyUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import static org.apache.myfaces.extensions.validator.util.ExtValAnnotationUtils.addPropertyAccessAnnotations;
import static org.apache.myfaces.extensions.validator.util.ExtValAnnotationUtils.addFieldAccessAnnotations;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * EXTVAL-59
 *
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class JoinValidationMetaDataStorageFilter implements MetaDataStorageFilter
{
    private static final String STATIC_SEPARATOR = ":";

    public void filter(PropertyInformation propertyInformation)
    {
        if (propertyInformation != null)
        {
            List<MetaDataEntry> result = new ArrayList<MetaDataEntry>();

            resolveJoinValidationMetaData(propertyInformation, result);

            propertyInformation.resetMetaDataEntries();

            setDefaultPropertyDetails(propertyInformation, result);

            if (containsJoinValidationConstraint(result))
            {
                filter(propertyInformation);
            }
        }
    }

    private void resolveJoinValidationMetaData(PropertyInformation propertyInformation, List<MetaDataEntry> result)
    {
        for (MetaDataEntry metaDataEntry : propertyInformation.getMetaDataEntries())
        {
            result.addAll(tryToTransformEntry(metaDataEntry));
        }
    }

    private void setDefaultPropertyDetails(PropertyInformation propertyInformation, List<MetaDataEntry> result)
    {
        for (MetaDataEntry metaDataEntry : result)
        {
            metaDataEntry.setProperty(PropertyInformationKeys.PROPERTY_DETAILS,
                    propertyInformation.getInformation(PropertyInformationKeys.PROPERTY_DETAILS));
            propertyInformation.addMetaDataEntry(metaDataEntry);
        }
    }

    private boolean containsJoinValidationConstraint(List<MetaDataEntry> result)
    {
        for (MetaDataEntry entry : result)
        {
            if (entry.getValue() instanceof JoinValidation)
            {
                return true;
            }
        }

        return false;
    }

    private List<MetaDataEntry> tryToTransformEntry(MetaDataEntry metaDataEntry)
    {
        List<MetaDataEntry> result = new ArrayList<MetaDataEntry>();

        if (metaDataEntry.getValue() instanceof JoinValidation)
        {
            JoinValidation annotation = metaDataEntry.getValue(JoinValidation.class);

            replaceMetaData(annotation, metaDataEntry, result);
        }
        else
        {
            result.add(metaDataEntry);
        }

        return result;
    }

    private void replaceMetaData(JoinValidation annotation, MetaDataEntry metaDataEntry, List<MetaDataEntry> result)
    {
        for (String target : annotation.value())
        {
            tryToReplaceMetaDataOfTarget(target, metaDataEntry, result);
        }
    }

    private void tryToReplaceMetaDataOfTarget(String target, MetaDataEntry metaDataEntry, List<MetaDataEntry> result)
    {
        try
        {
            if (isStaticSyntax(target))
            {
                addMetaData(result, extractStaticMetaData(target));
            }
            else
            {
                addMetaData(result, extractDynamicMetaData(metaDataEntry, target));
            }
        }
        catch (Exception e)
        {
            //do nothing a different filter might introduce a new syntax which causes the exception
        }
    }

    private void addMetaData(List<MetaDataEntry> result, MetaDataEntry[] metaDataEntries)
    {
        Collections.addAll(result, metaDataEntries);
    }

    private boolean isStaticSyntax(String target)
    {
        return target.contains(STATIC_SEPARATOR);
    }

    private MetaDataEntry[] extractStaticMetaData(String target)
    {
        int separatorIndex = target.lastIndexOf(STATIC_SEPARATOR);

        Class targetClass = loadClass(target.substring(0, separatorIndex));
        String propertyName = target.substring(separatorIndex + 1);

        return new StaticSyntaxMetaDataExtractor().extract(targetClass, propertyName).getMetaDataEntries();
    }

    private Class loadClass(String className)
    {
        try
        {
            return ClassUtils.loadClassForName(className);
        }
        catch (ClassNotFoundException e)
        {
            throw new IllegalArgumentException(e);
        }
    }

    private MetaDataEntry[] extractDynamicMetaData(MetaDataEntry metaDataEntry, String target)
    {
        PropertyDetails propertyDetails = ExtValUtils.createPropertyDetailsForNewTarget(metaDataEntry, target);

        Class targetClass = ProxyUtils.getUnproxiedClass(propertyDetails.getBaseObject().getClass());
        return new StaticSyntaxMetaDataExtractor().extract(
                targetClass, propertyDetails.getProperty()).getMetaDataEntries();
    }

    private class StaticSyntaxMetaDataExtractor extends DefaultComponentMetaDataExtractor
    {
        public PropertyInformation extract(Class targetClass, String targetProperty)
        {
            PropertyInformation propertyInformation = new DefaultPropertyInformation();

            PropertyStorage storage = ReflectionUtils.getPropertyStorage();
            addPropertyAccessAnnotations(storage, targetClass, targetProperty, propertyInformation);
            addFieldAccessAnnotations(storage, targetClass, targetProperty, propertyInformation);

            return propertyInformation;
        }
    }

    @Override
    public boolean equals(Object obj)
    {
        return obj != null && getClass().equals(obj.getClass());
    }

    @Override
    public int hashCode()
    {
        return super.hashCode();
    }
}
