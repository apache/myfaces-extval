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
package org.apache.myfaces.extensions.validator.core.validation.strategy;

import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.factory.AbstractNameMapperAwareFactory;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.CustomInformation;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationEntry;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationNames;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticResourceBundleConfiguration;
import org.apache.myfaces.extensions.validator.core.validation.strategy.mapper
    .AnnotationToValidationStrategyBeanNameMapper;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.NullValueAwareConcurrentHashMap;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.util.List;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.logging.Logger;


/**
 * Factory which creates the ValidationStrategy for a given Meta-Data Key
 *
 * @since 1.x.1
 */
@UsageInformation({UsageCategory.INTERNAL, UsageCategory.CUSTOMIZABLE})
public class DefaultValidationStrategyFactory extends AbstractNameMapperAwareFactory<String>
        implements ClassMappingFactory<String, ValidationStrategy>
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    private Map<String, String> metaDataKeyToValidationStrategyMapping = null;
    private List<NameMapper<String>> nameMapperList = new CopyOnWriteArrayList<NameMapper<String>>();

    public DefaultValidationStrategyFactory()
    {
        logger.fine(getClass().getName() + " instantiated");
    }

    public ValidationStrategy create(String metaDataKey)
    {
        if (metaDataKeyToValidationStrategyMapping == null)
        {
            initStaticMappings();
        }

        if (metaDataKeyToValidationStrategyMapping.containsKey(metaDataKey))
        {
            return getValidationStrategyInstance(metaDataKeyToValidationStrategyMapping.get(metaDataKey));
        }

        ValidationStrategy validationStrategy;
        String strategyName;
        //null -> use name mappers
        for (NameMapper<String> nameMapper : nameMapperList)
        {
            strategyName = nameMapper.createName(metaDataKey);

            if (strategyName == null)
            {
                continue;
            }

            validationStrategy = getValidationStrategyInstance(strategyName);

            if (validationStrategy != null)
            {
                addMapping(metaDataKey, strategyName);
                return validationStrategy;
            }
        }

        addMapping(metaDataKey, null);
        return null;
    }

    private ValidationStrategy getValidationStrategyInstance(String validationStrategyName)
    {
        if(validationStrategyName == null)
        {
            return null;
        }

        if (validationStrategyName
            .startsWith(AnnotationToValidationStrategyBeanNameMapper.PREFIX_FOR_BEAN_MAPPING))
        {
            return (ValidationStrategy) ExtValUtils.getELHelper().getBean(validationStrategyName
                    .substring(AnnotationToValidationStrategyBeanNameMapper.PREFIX_FOR_BEAN_MAPPING.length()));
        }
        else
        {
            return (ValidationStrategy) ClassUtils.tryToInstantiateClassForName(validationStrategyName);
        }
    }

    private synchronized void addMapping(String metaDataKey, String validationStrategyName)
    {
        logger.finest("adding meta-data key to validation strategy mapping: "
            + metaDataKey + " -> " + validationStrategyName);

        metaDataKeyToValidationStrategyMapping.put(metaDataKey, validationStrategyName);
    }

    @ToDo(value = Priority.MEDIUM, description = "logging")
    private synchronized void initStaticMappings()
    {
        metaDataKeyToValidationStrategyMapping = new NullValueAwareConcurrentHashMap<String, String>(String.class);

        //setup internal static mappings
        for (StaticConfiguration<String, String> staticConfig :
            ExtValContext.getContext().getStaticConfiguration(
                StaticConfigurationNames.META_DATA_TO_VALIDATION_STRATEGY_CONFIG))
        {
            setupStrategyMappings(staticConfig.getMapping());
        }

        StaticConfiguration<String, String> staticConfig = new StaticResourceBundleConfiguration();
        //try to setup mapping with base name by convention - overrides default mapping
        try
        {
            //build convention (strategy mapping)
            staticConfig.setSourceOfMapping(ExtValContext.getContext().getInformationProviderBean()
                .get(CustomInformation.STATIC_STRATEGY_MAPPING_SOURCE));

            setupStrategyMappings(staticConfig.getMapping());
        }
        catch (Exception e)
        {
            //do nothing - it was just a try
        }

        //setup custom mapping - overrides all other mappings
        String customMappingBaseName = ExtValCoreConfiguration.get().customStaticValidationStrategyMappingSource();
        if (customMappingBaseName != null)
        {
            try
            {
                staticConfig = new StaticResourceBundleConfiguration();
                staticConfig.setSourceOfMapping(customMappingBaseName);
                setupStrategyMappings(staticConfig.getMapping());
            }
            catch (MissingResourceException e)
            {
                e.printStackTrace();
            }
        }
    }

    private void setupStrategyMappings(List<StaticConfigurationEntry<String,String>> mappings)
    {
        for(StaticConfigurationEntry<String, String> mapping : mappings)
        {
            addMapping(mapping.getSource(), mapping.getTarget());
        }
    }

    protected List<NameMapper<String>> getNameMapperList()
    {
        return this.nameMapperList;
    }
}
