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
package org.apache.myfaces.extensions.validator.util;

import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.ValidationModuleKey;
import org.apache.myfaces.extensions.validator.core.ProjectStageName;
import org.apache.myfaces.extensions.validator.core.el.AbstractELHelperFactory;
import org.apache.myfaces.extensions.validator.core.el.ELHelper;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.factory.FacesMessageFactory;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.factory.NameMapperAwareFactory;
import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationEntry;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationNames;
import org.apache.myfaces.extensions.validator.core.interceptor.MetaDataExtractionInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.PropertyValidationInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.ValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.ComponentMetaDataExtractorFactory;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.storage.FacesMessageStorage;
import org.apache.myfaces.extensions.validator.core.storage.StorageManager;
import org.apache.myfaces.extensions.validator.core.validation.SkipValidationEvaluator;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.MessageResolver;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractor;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractorFactory;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ViolationSeverityInterpreter;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.ExtValInformation;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;
import java.util.Map;
import java.util.HashMap;
import java.util.MissingResourceException;
import java.util.List;
import java.util.ArrayList;
import java.util.logging.Logger;
import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@SuppressWarnings({"unchecked"})
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValUtils
{
    private static final Logger LOGGER = Logger.getLogger(ExtValUtils.class.getName());

    private static final String JAVAX_FACES_REQUIRED = "javax.faces.component.UIInput.REQUIRED";
    private static final String JAVAX_FACES_REQUIRED_DETAIL = "javax.faces.component.UIInput.REQUIRED_detail";

    private static final String JAVAX_FACES_MAXIMUM = "javax.faces.validator.LengthValidator.MAXIMUM";
    private static final String JAVAX_FACES_MAXIMUM_DETAIL = "javax.faces.validator.LengthValidator.MAXIMUM_detail";

    public static ValidationStrategy getValidationStrategyForMetaData(String metaDataKey)
    {
        return ((ClassMappingFactory<String, ValidationStrategy>) ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.VALIDATION_STRATEGY_FACTORY, ClassMappingFactory.class))
                .create(metaDataKey);
    }

    public static void registerMetaDataToValidationStrategyNameMapper(
            NameMapper<String> metaDataToValidationStrategyNameMapper)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.VALIDATION_STRATEGY_FACTORY, NameMapperAwareFactory.class))
                .register(metaDataToValidationStrategyNameMapper);
    }

    public static void deregisterMetaDataToValidationStrategyNameMapper(
            Class<? extends NameMapper> metaDataToValidationStrategyNameMapperClass)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.VALIDATION_STRATEGY_FACTORY, NameMapperAwareFactory.class))
                .deregister(metaDataToValidationStrategyNameMapperClass);
    }

    public static void denyMetaDataToValidationStrategyNameMapper(
            Class<? extends NameMapper> metaDataToValidationStrategyNameMapperClass)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.VALIDATION_STRATEGY_FACTORY, NameMapperAwareFactory.class))
                .deny(metaDataToValidationStrategyNameMapperClass);
    }

    public static MetaDataTransformer getMetaDataTransformerForValidationStrategy(ValidationStrategy validationStrategy)
    {
        return ((ClassMappingFactory<ValidationStrategy, MetaDataTransformer>) ExtValContext
                .getContext().getFactoryFinder()
                .getFactory(FactoryNames.META_DATA_TRANSFORMER_FACTORY, ClassMappingFactory.class))
                .create(validationStrategy);
    }

    public static void registerValidationStrategyToMetaDataTransformerNameMapper(
            NameMapper<ValidationStrategy> validationStrategyToMetaDataTransformerNameMapper)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.META_DATA_TRANSFORMER_FACTORY, NameMapperAwareFactory.class))
                .register(validationStrategyToMetaDataTransformerNameMapper);
    }

    public static void deregisterValidationStrategyToMetaDataTransformerNameMapper(
            Class<? extends NameMapper> validationStrategyToMetaDataTransformerNameMapperClass)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.META_DATA_TRANSFORMER_FACTORY, NameMapperAwareFactory.class))
                .deregister(validationStrategyToMetaDataTransformerNameMapperClass);
    }

    public static void denyValidationStrategyToMetaDataTransformerNameMapper(
            Class<? extends NameMapper> validationStrategyToMetaDataTransformerNameMapperClass)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.META_DATA_TRANSFORMER_FACTORY, NameMapperAwareFactory.class))
                .deny(validationStrategyToMetaDataTransformerNameMapperClass);
    }

    public static MetaDataExtractor getComponentMetaDataExtractor()
    {
        return ExtValContext.getContext().getFactoryFinder()
                .getFactory(FactoryNames.COMPONENT_META_DATA_EXTRACTOR_FACTORY, ComponentMetaDataExtractorFactory.class)
                .create();
    }

    public static MetaDataExtractor getComponentMetaDataExtractorWith(Map<String, Object> properties)
    {
        return ExtValContext.getContext().getFactoryFinder()
                .getFactory(FactoryNames.COMPONENT_META_DATA_EXTRACTOR_FACTORY, ComponentMetaDataExtractorFactory.class)
                .createWith(properties);
    }

    public static void configureComponentWithMetaData(FacesContext facesContext,
                                                      UIComponent uiComponent,
                                                      Map<String, Object> metaData)
    {
        for (ComponentInitializer componentInitializer : ExtValContext.getContext().getComponentInitializers())
        {
            componentInitializer.configureComponent(facesContext, uiComponent, metaData);
        }
    }

    public static boolean executeAfterThrowingInterceptors(UIComponent uiComponent,
                                                           MetaDataEntry metaDataEntry,
                                                           Object convertedObject,
                                                           ValidatorException validatorException,
                                                           ValidationStrategy validatorExceptionSource)
    {
        boolean result = true;

        if (metaDataEntry == null)
        {
            metaDataEntry = new MetaDataEntry();
        }

        for (ValidationExceptionInterceptor validationExceptionInterceptor : ExtValContext.getContext()
                .getValidationExceptionInterceptors())
        {
            if (!validationExceptionInterceptor.afterThrowing(
                    uiComponent, metaDataEntry, convertedObject, validatorException, validatorExceptionSource))
            {
                result = false;
            }
        }

        return result;
    }

    public static MetaDataExtractor createInterceptedMetaDataExtractor(final MetaDataExtractor metaDataExtractor)
    {
        return createInterceptedMetaDataExtractorWith(metaDataExtractor, null);
    }

    public static MetaDataExtractor createInterceptedMetaDataExtractorFor(
            final MetaDataExtractor metaDataExtractor, Class moduleKey)
    {
        Map<String, Object> properties = new HashMap<String, Object>();

        if(moduleKey != null)
        {
            properties.put(ValidationModuleKey.class.getName(), moduleKey);
        }
        return createInterceptedMetaDataExtractorWith(metaDataExtractor, properties);
    }

    public static MetaDataExtractor createInterceptedMetaDataExtractorWith(
            final MetaDataExtractor metaDataExtractor, final Map<String, Object> properties)
    {
        return new MetaDataExtractor()
        {
            public PropertyInformation extract(FacesContext facesContext, Object object)
            {
                PropertyInformation result = metaDataExtractor.extract(facesContext, object);

                addProperties(result, properties);
                invokeMetaDataExtractionInterceptors(result, properties);

                return result;
            }
        };
    }

    private static void addProperties(PropertyInformation result, Map<String, Object> properties)
    {
        if(properties != null)
        {
            Map<String, Object> customProperties = getCustomProperties(result);
            customProperties.putAll(properties);
        }
    }

    private static Map<String, Object> getCustomProperties(PropertyInformation propertyInformation)
    {
        if(!propertyInformation.containsInformation(PropertyInformationKeys.CUSTOM_PROPERTIES))
        {
            propertyInformation.setInformation(
                    PropertyInformationKeys.CUSTOM_PROPERTIES, new HashMap<String, Object>());
        }

        return (Map<String, Object>) propertyInformation.getInformation(PropertyInformationKeys.CUSTOM_PROPERTIES);
    }

    private static void invokeMetaDataExtractionInterceptors(PropertyInformation result, Map<String, Object> properties)
    {
        for (MetaDataExtractionInterceptor metaDataExtractionInterceptor :
                ExtValContext.getContext().getMetaDataExtractionInterceptorsWith(properties))
        {
            metaDataExtractionInterceptor.afterExtracting(result);
        }
    }

    public static MessageResolver getMessageResolverForValidationStrategy(ValidationStrategy validationStrategy)
    {
        return ((ClassMappingFactory<ValidationStrategy, MessageResolver>) ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.MESSAGE_RESOLVER_FACTORY, ClassMappingFactory.class))
                .create(validationStrategy);
    }

    public static void registerValidationStrategyToMessageResolverNameMapper(
            NameMapper<ValidationStrategy> validationStrategyToMsgResolverNameMapper)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.MESSAGE_RESOLVER_FACTORY, NameMapperAwareFactory.class))
                .register(validationStrategyToMsgResolverNameMapper);
    }

    public static void deregisterValidationStrategyToMessageResolverNameMapper(
            Class<? extends NameMapper> validationStrategyToMessageResolverNameMapperClass)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.MESSAGE_RESOLVER_FACTORY, NameMapperAwareFactory.class))
                .deregister(validationStrategyToMessageResolverNameMapperClass);
    }

    public static void denyValidationStrategyToMessageResolverNameMapper(
            Class<? extends NameMapper> validationStrategyToMessageResolverNameMapperClass)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.MESSAGE_RESOLVER_FACTORY, NameMapperAwareFactory.class))
                .deny(validationStrategyToMessageResolverNameMapperClass);
    }

    public static ELHelper getELHelper()
    {
        return ExtValContext.getContext().getFactoryFinder()
                .getFactory(FactoryNames.EL_HELPER_FACTORY, AbstractELHelperFactory.class).create();
    }

    public static FacesMessage createFacesMessage(String summary, String detail)
    {
        return createFacesMessage(FacesMessage.SEVERITY_ERROR, summary, detail);
    }

    public static FacesMessage createFacesMessage(FacesMessage.Severity severity, String summary, String detail)
    {
        return ExtValContext.getContext().getFactoryFinder()
                .getFactory(FactoryNames.FACES_MESSAGE_FACTORY, FacesMessageFactory.class)
                .create(severity, summary, detail);
    }

    public static FacesMessage convertFacesMessage(FacesMessage facesMessage)
    {
        return ExtValContext.getContext().getFactoryFinder()
                .getFactory(FactoryNames.FACES_MESSAGE_FACTORY, FacesMessageFactory.class)
                .convert(facesMessage);
    }

    public static PropertyDetails createPropertyDetailsForNewTarget(MetaDataEntry metaDataEntry,
                                                                    String targetExpression)
    {
        Object baseObject;
        if (getELHelper().isELTermWellFormed(targetExpression))
        {
            ValueBindingExpression vbe = new ValueBindingExpression(targetExpression);

            String expression = vbe.getExpressionString();
            baseObject = getELHelper().getValueOfExpression(FacesContext.getCurrentInstance(), vbe.getBaseExpression());
            return new PropertyDetails(
                    expression.substring(2, expression.length() - 1), baseObject, vbe.getProperty());
        }

        PropertyDetails original = metaDataEntry.getProperty(
                PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class);

        String newBaseKey = original.getKey().substring(0, original.getKey().lastIndexOf(".") + 1);
        String newKey = newBaseKey + targetExpression;

        baseObject = ReflectionUtils.getBaseOfPropertyChain(original.getBaseObject(), targetExpression);
        return new PropertyDetails(
                newKey, baseObject, targetExpression.substring(targetExpression.lastIndexOf(".") + 1,
                        targetExpression.length()));
    }

    @UsageInformation(UsageCategory.INTERNAL)
    public static void tryToPlaceLabel(FacesMessage facesMessage, String label, int index)
    {
        if (facesMessage.getSummary() != null && facesMessage.getSummary().contains("{" + index + "}"))
        {
            facesMessage.setSummary(facesMessage.getSummary().replace("{" + index + "}", label));
        }

        if (facesMessage.getDetail() != null && facesMessage.getDetail().contains("{" + index + "}"))
        {
            facesMessage.setDetail(facesMessage.getDetail().replace("{" + index + "}", label));
        }
    }

    @UsageInformation(UsageCategory.INTERNAL)
    public static void replaceWithDefaultMaximumMessage(FacesMessage facesMessage, int maxLength)
    {
        String facesRequiredMessage = JsfUtils.getMessageFromApplicationMessageBundle(JAVAX_FACES_MAXIMUM);
        String facesRequiredMessageDetail = facesRequiredMessage;

        //use try/catch for easier sync between trunk/branch
        try
        {
            if (JsfUtils.getMessageFromApplicationMessageBundle(JAVAX_FACES_MAXIMUM_DETAIL) != null)
            {
                facesRequiredMessageDetail = JsfUtils
                        .getMessageFromApplicationMessageBundle(JAVAX_FACES_MAXIMUM_DETAIL);
            }
        }
        catch (MissingResourceException missingResourceException)
        {
            //jsf 1.2 doesn't have a detail message
        }

        facesRequiredMessage = facesRequiredMessage.replace("{0}", "" + maxLength);
        facesRequiredMessageDetail = facesRequiredMessageDetail.replace("{0}", "" + maxLength);

        facesMessage.setSummary(facesRequiredMessage);
        facesMessage.setDetail(facesRequiredMessageDetail);
    }

    @UsageInformation(UsageCategory.INTERNAL)
    public static void replaceWithDefaultRequiredMessage(FacesMessage facesMessage)
    {
        String facesRequiredMessage = JsfUtils.getMessageFromApplicationMessageBundle(JAVAX_FACES_REQUIRED);
        String facesRequiredMessageDetail = facesRequiredMessage;

        //use try/catch for easier sync between trunk/branch
        try
        {
            if (JsfUtils.getMessageFromApplicationMessageBundle(JAVAX_FACES_REQUIRED_DETAIL) != null)
            {
                facesRequiredMessageDetail = JsfUtils
                        .getMessageFromApplicationMessageBundle(JAVAX_FACES_REQUIRED_DETAIL);
            }
        }
        catch (MissingResourceException missingResourceException)
        {
            //jsf 1.2 doesn't have a detail message
        }

        facesMessage.setSummary(facesRequiredMessage);
        facesMessage.setDetail(facesRequiredMessageDetail);
    }

    public static boolean isSkipableValidationStrategy(Class<? extends ValidationStrategy> targetClass)
    {
        for (Class currentClass : getSkipValidationSupportClassList())
        {
            if (isSkipValidationSupported(currentClass, targetClass))
            {
                return true;
            }
        }

        return false;
    }

    public static boolean processMetaDataEntryAfterSkipValidation(
            Class<? extends ValidationStrategy> targetClass, MetaDataEntry entry)
    {
        return isSkipableValidationStrategy(targetClass) &&
                Boolean.TRUE.equals(entry.getProperty(PropertyInformationKeys.SKIP_VALIDATION, Boolean.class));
    }

    public static List<Class> getSkipValidationSupportClassList()
    {
        List<StaticConfiguration<String, String>> staticConfigurationList = ExtValContext.getContext()
                .getStaticConfiguration(StaticConfigurationNames.SKIP_VALIDATION_SUPPORT_CONFIG);

        List<Class> markerList = new ArrayList<Class>();

        Class currentClass;
        for (StaticConfiguration<String, String> currentEntry : staticConfigurationList)
        {
            for (StaticConfigurationEntry<String, String> currentConfigurationEntry : currentEntry.getMapping())
            {
                currentClass = ClassUtils.tryToLoadClassForName(currentConfigurationEntry.getTarget());

                if (currentClass != null)
                {
                    markerList.add(currentClass);
                }
                else
                {
                    LOGGER.warning("configuration entry provides an invalid entry: "
                            + currentConfigurationEntry.getTarget());
                }
            }
        }

        return markerList;
    }

    @SuppressWarnings({"unchecked"})
    public static boolean isSkipValidationSupported(Class currentClass, Class targetClass)
    {
        if (currentClass.isAnnotation())
        {
            if (targetClass.isAnnotationPresent(currentClass))
            {
                return true;
            }
        }
        else
        {
            if (currentClass.isAssignableFrom(targetClass))
            {
                return true;
            }
        }

        return false;
    }

    public static ValidationParameterExtractor getValidationParameterExtractor()
    {
        if(isValidationParameterExtractionDeactivated())
        {
            return new ValidationParameterExtractor() {

                public Map<Object, List<Object>> extract(Annotation annotation)
                {
                    return new HashMap<Object, List<Object>>();
                }

                public List<Object> extract(Annotation annotation, Object key)
                {
                    return new ArrayList<Object>();
                }

                public <T> List<T> extract(Annotation annotation, Object key, Class<T> valueType)
                {
                    return new ArrayList<T>();
                }

                public <T> T extract(Annotation annotation, Object key, Class<T> valueType, Class valueId)
                {
                    return null;
                }
            };
        }
        return ExtValContext.getContext().getFactoryFinder().getFactory(
                FactoryNames.VALIDATION_PARAMETER_EXTRACTOR_FACTORY, ValidationParameterExtractorFactory.class)
                .create();
    }

    public static Class getValidationParameterClassFor(Class source)
    {
         ClassMappingFactory<Class, Class> validationParameterFactory = ExtValContext.getContext().getFactoryFinder()
                 .getFactory(FactoryNames.VALIDATION_PARAMETER_FACTORY, ClassMappingFactory.class);

        return validationParameterFactory.create(source);
    }

    private static boolean isValidationParameterExtractionDeactivated()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_VALIDATION_PARAMETERS);
    }

    public static boolean executeLocalBeforeValidationInterceptors(FacesContext facesContext,
                                                                   UIComponent uiComponent,
                                                                   Object convertedObject,
                                                                   String propertyKey,
                                                                   Object properties,
                                                                   Annotation annotation)
    {
        Map<String, Object> propertyMap = new HashMap<String, Object>();
        List<PropertyValidationInterceptor> propertyValidationInterceptors = getValidationParameterExtractor().extract(
                annotation, PropertyValidationInterceptor.class, PropertyValidationInterceptor.class);
        boolean result = true;

        if (properties != null)
        {
            propertyMap.put(propertyKey, properties);
        }
        propertyMap.put(Annotation.class.getName(), annotation);

        for (PropertyValidationInterceptor propertyValidationInterceptor : propertyValidationInterceptors)
        {
            if (!propertyValidationInterceptor
                    .beforeValidation(facesContext, uiComponent, convertedObject, propertyMap))
            {
                result = false;
            }
        }

        return result;
    }

    public static void executeLocalAfterValidationInterceptors(FacesContext facesContext,
                                                               UIComponent uiComponent,
                                                               Object convertedObject,
                                                               String propertyKey,
                                                               Object properties,
                                                               Annotation annotation)
    {
        Map<String, Object> propertyMap = new HashMap<String, Object>();
        List<PropertyValidationInterceptor> propertyValidationInterceptors = getValidationParameterExtractor().extract(
                annotation, PropertyValidationInterceptor.class, PropertyValidationInterceptor.class);

        if (properties != null)
        {
            propertyMap.put(propertyKey, properties);
            propertyMap.put(Annotation.class.getName(), annotation);
        }

        for (PropertyValidationInterceptor propertyValidationInterceptor : propertyValidationInterceptors)
        {
            propertyValidationInterceptor.afterValidation(facesContext, uiComponent, convertedObject, propertyMap);
        }
    }

    @ToDo(value = Priority.MEDIUM, description = "is renaming ok?")
    public static boolean executeGlobalBeforeValidationInterceptors(FacesContext facesContext,
                                                                    UIComponent uiComponent,
                                                                    Object convertedObject,
                                                                    String propertyKey,
                                                                    Object properties,
                                                                    Class moduleKey)
    {
        Map<String, Object> propertyMap = new HashMap<String, Object>();
        boolean result = true;

        if (properties != null)
        {
            propertyMap.put(propertyKey, properties);
        }

        List<PropertyValidationInterceptor> propertyValidationInterceptors =
                ExtValContext.getContext().getPropertyValidationInterceptorsFor(moduleKey);

        for (PropertyValidationInterceptor propertyValidationInterceptor : propertyValidationInterceptors)
        {
            if (!propertyValidationInterceptor
                    .beforeValidation(facesContext, uiComponent, convertedObject, propertyMap))
            {
                result = false;
            }
        }

        return result;
    }

    @ToDo(value = Priority.MEDIUM, description = "is renaming ok?")
    public static void executeGlobalAfterValidationInterceptors(FacesContext facesContext,
                                                                UIComponent uiComponent,
                                                                Object convertedObject,
                                                                String propertyKey,
                                                                Object properties,
                                                                Class moduleKey)
    {
        Map<String, Object> propertyMap = new HashMap<String, Object>();

        if (properties != null)
        {
            propertyMap.put(propertyKey, properties);
        }

        List<PropertyValidationInterceptor> propertyValidationInterceptors =
                ExtValContext.getContext().getPropertyValidationInterceptorsFor(moduleKey);

        for (PropertyValidationInterceptor propertyValidationInterceptor : propertyValidationInterceptors)
        {
            propertyValidationInterceptor.afterValidation(facesContext, uiComponent, convertedObject, propertyMap);
        }
    }

    public static <T> T getStorage(Class<T> storageType, String storageName)
    {
        T storage = getCachedStorage(storageType, storageName);

        if(storage != null)
        {
            return storage;
        }
        storage = (T) getStorageManagerFactory().create(storageType).create(storageName);

        cacheStorageForRequest(storageType, storageName, storage);

        return storage;
    }

    private static <T> T getCachedStorage(Class<T> storageType, String storageName)
    {
        Map requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();

        String key = createStorageKey(storageType.getName(), storageName);
        return (T)requestMap.get(key);
    }

    private static <T> void cacheStorageForRequest(Class<T> storageType, String storageName, T storage)
    {
        Map requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();

        String key = createStorageKey(storageType.getName(), storageName);
        requestMap.put(key, storage);
    }

    private static String createStorageKey(String storageType, String storageName)
    {
        StringBuilder key = new StringBuilder("cachedStorage:");
        key.append(storageType);
        key.append(":");
        key.append(storageName);
        return key.toString();
    }

    public static void resetStorage(Class storageType, String storageName)
    {
        resetCachedStorage(storageType.getName(), storageName);

        getStorageManagerFactory().create(storageType).reset(storageName);
    }

    private static void resetCachedStorage(String storageType, String storageName)
    {
        String key = createStorageKey(storageType, storageName);

        Map requestMap = FacesContext.getCurrentInstance().getExternalContext().getRequestMap();
        requestMap.put(key, null);
    }

    private static ClassMappingFactory<Class, StorageManager> getStorageManagerFactory()
    {
        return (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.STORAGE_MANAGER_FACTORY, ClassMappingFactory.class));
    }

    public static Map<String, Object> getTransformedMetaData(FacesContext facesContext, UIComponent uiComponent)
    {
        return getTransformedMetaDataFor(facesContext, uiComponent, null);
    }

    public static Map<String, Object> getTransformedMetaDataFor(
            FacesContext facesContext, UIComponent uiComponent, Class moduleKey)
    {
        Map<String, Object> properties = new HashMap<String, Object>();

        if(moduleKey != null)
        {
            properties.put(ValidationModuleKey.class.getName(), moduleKey);
        }

        return getTransformedMetaDataWith(facesContext, uiComponent, properties);
    }

    public static Map<String, Object> getTransformedMetaDataFor(
            FacesContext facesContext, PropertyInformation propertyInformation, Class moduleKey)
    {
        Map<String, Object> properties = new HashMap<String, Object>();

        if (moduleKey != null)
        {
            properties.put(ValidationModuleKey.class.getName(), moduleKey);
        }

        return getTransformedMetaDataWith(facesContext, propertyInformation, properties);
    }

    public static Map<String, Object> getTransformedMetaDataWith(
            FacesContext facesContext, UIComponent uiComponent, Map<String, Object> properties)
    {
        ValidationStrategy validationStrategy;

        SkipValidationEvaluator skipValidationEvaluator = ExtValContext.getContext().getSkipValidationEvaluator();
        MetaDataExtractor metaDataExtractor = getComponentMetaDataExtractorWith(properties);

        Map<String, Object> metaData;
        Map<String, Object> metaDataResult = new HashMap<String, Object>();

        for (MetaDataEntry entry : metaDataExtractor.extract(facesContext, uiComponent).getMetaDataEntries())
        {
            metaData = new HashMap<String, Object>();
            validationStrategy = getValidationStrategyForMetaData(entry.getKey());

            if (validationStrategy != null)
            {
                metaData = transformMetaData(
                        facesContext, uiComponent, validationStrategy, skipValidationEvaluator, metaData, entry);

                if (!isComponentInitializationSkipped(metaData, entry, validationStrategy))
                {
                    //don't break maybe there are constraints which don't support the skip-mechanism
                    metaDataResult.putAll(metaData);
                }
            }
        }

        return metaDataResult;
    }

    public static Map<String, Object> getTransformedMetaDataWith(FacesContext facesContext,
                                                                 PropertyInformation propertyInformation,
                                                                 Map<String, Object> properties)
    {
        ValidationStrategy validationStrategy;

        // This is called as part of the MetaData extraction in the default process. 
        // So we need to do it here also before the transformations are run.
        for (MetaDataExtractionInterceptor metaDataExtractionInterceptor :
                ExtValContext.getContext().getMetaDataExtractionInterceptorsWith(properties))
        {
            metaDataExtractionInterceptor.afterExtracting(propertyInformation);
        }

        SkipValidationEvaluator skipValidationEvaluator = ExtValContext.getContext().getSkipValidationEvaluator();

        Map<String, Object> metaData;
        Map<String, Object> metaDataResult = new HashMap<String, Object>();

        for (MetaDataEntry entry : propertyInformation.getMetaDataEntries())
        {
            metaData = new HashMap<String, Object>();
            validationStrategy = getValidationStrategyForMetaData(entry.getKey());

            if (validationStrategy != null)
            {
                metaData = transformMetaData(
                        facesContext, null, validationStrategy, skipValidationEvaluator, metaData, entry);

                if (!isComponentInitializationSkipped(metaData, entry, validationStrategy))
                {
                    //don't break maybe there are constraints which don't support the skip-mechanism
                    metaDataResult.putAll(metaData);
                }
            }
        }

        return metaDataResult;
    }

    private static Map<String, Object> transformMetaData(FacesContext facesContext, UIComponent uiComponent,
                                                         ValidationStrategy validationStrategy,
                                                         SkipValidationEvaluator skipValidationEvaluator,
                                                         Map<String, Object> metaData, MetaDataEntry entry)
    {
        if (!skipValidationEvaluator.skipValidation(facesContext, uiComponent, validationStrategy, entry))
        {
            MetaDataTransformer metaDataTransformer = getMetaDataTransformerForValidationStrategy(validationStrategy);

            if (metaDataTransformer != null)
            {
                LOGGER.fine(metaDataTransformer.getClass().getName() + " instantiated");

                metaData = metaDataTransformer.convertMetaData(entry);
            }
            else
            {
                metaData = null;
            }

            if (metaData == null)
            {
                return new HashMap<String, Object>();
            }
        }
        return metaData;
    }

    private static boolean isComponentInitializationSkipped(Map<String, Object> metaData, MetaDataEntry entry,
                                                            ValidationStrategy validationStrategy)
    {
        return metaData.isEmpty() ||
                (Boolean.TRUE.equals(entry.getProperty(PropertyInformationKeys.SKIP_VALIDATION, Boolean.class)) &&
                        isSkipableValidationStrategy(ProxyUtils.getUnproxiedClass(
                                validationStrategy.getClass(), ValidationStrategy.class)));
    }

    public static boolean interpretEmptyStringValuesAsNull()
    {
        //to deactivate: the parameter has to be explicitly false
        return !"false".equalsIgnoreCase(WebXmlParameter.INTERPRET_EMPTY_STRING_SUBMITTED_VALUES_AS_NULL);
    }

    public static boolean validateEmptyFields()
    {
        return !"false".equalsIgnoreCase(WebXmlParameter.VALIDATE_EMPTY_FIELDS);
    }

    public static PropertyDetails getPropertyDetails(PropertyInformation propertyInformation)
    {
        return propertyInformation.getInformation(PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class);
    }

    public static void tryToThrowValidatorExceptionForComponentId(
            String clientId, FacesMessage facesMessage, Throwable throwable)
    {
        UIComponent targetComponent = findComponent(clientId);

        tryToThrowValidatorExceptionForComponent(targetComponent, facesMessage, throwable);
    }

    public static void tryToThrowValidatorExceptionForComponent(
            UIComponent uiComponent, FacesMessage facesMessage, Throwable throwable)
    {
        ViolationSeverityInterpreter interpreter =
                ExtValContext.getContext().getViolationSeverityInterpreter();

        FacesContext facesContext = FacesContext.getCurrentInstance();

        if (interpreter.severityCausesValidatorException(facesContext, uiComponent, facesMessage.getSeverity()))
        {
            if (throwable == null)
            {
                throw new ValidatorException(facesMessage);
            }
            else
            {
                throw new ValidatorException(facesMessage, throwable);
            }
        }
        else
        {
            tryToAddViolationMessageForComponent(uiComponent, facesMessage);
        }
    }

    public static void tryToAddViolationMessageForComponentId(String clientId, FacesMessage facesMessage)
    {
        UIComponent targetComponent = findComponent(clientId);

        if (targetComponent == null && clientId != null)
        {
            tryToAddViolationMessageForTestClientId(clientId, facesMessage);
            return;
        }
        tryToAddViolationMessageForComponent(targetComponent, facesMessage);
    }

    @ToDo(value = Priority.MEDIUM, description = "required for test frameworks - goal: remove it")
    private static void tryToAddViolationMessageForTestClientId(String clientId, FacesMessage facesMessage)
    {
        ViolationSeverityInterpreter interpreter =
                ExtValContext.getContext().getViolationSeverityInterpreter();

        FacesContext facesContext = FacesContext.getCurrentInstance();

        if (interpreter.severityCausesViolationMessage(facesContext, null, facesMessage.getSeverity()))
        {
            addFacesMessage(clientId, facesMessage);
        }
        tryToBlocksNavigationForComponent(null, facesMessage);
    }

    public static void tryToAddViolationMessageForComponent(UIComponent uiComponent, FacesMessage facesMessage)
    {
        ViolationSeverityInterpreter interpreter =
                ExtValContext.getContext().getViolationSeverityInterpreter();

        FacesContext facesContext = FacesContext.getCurrentInstance();

        if (interpreter.severityCausesViolationMessage(facesContext, uiComponent, facesMessage.getSeverity()))
        {
            if (uiComponent != null)
            {
                addFacesMessage(uiComponent.getClientId(facesContext), facesMessage);
            }
            else
            {
                addFacesMessage(null, facesMessage);
            }
        }
        tryToBlocksNavigationForComponent(uiComponent, facesMessage);
    }

    public static void addFacesMessage(FacesMessage facesMessage)
    {
        addFacesMessage(null, facesMessage);
    }

    public static void addFacesMessage(String clientId, FacesMessage facesMessage)
    {
        FacesMessageStorage storage = getStorage(FacesMessageStorage.class, FacesMessageStorage.class.getName());

        if (storage != null)
        {
            storage.addFacesMessage(clientId, facesMessage);
        }
        else
        {
            FacesContext.getCurrentInstance().addMessage(clientId, facesMessage);
        }
    }

    public static void tryToBlocksNavigationForComponentId(String clientId, FacesMessage facesMessage)
    {
        UIComponent targetComponent = findComponent(clientId);

        tryToBlocksNavigationForComponent(targetComponent, facesMessage);
    }

    public static void tryToBlocksNavigationForComponent(UIComponent uiComponent, FacesMessage facesMessage)
    {
        ViolationSeverityInterpreter interpreter =
                ExtValContext.getContext().getViolationSeverityInterpreter();

        FacesContext facesContext = FacesContext.getCurrentInstance();

        if (interpreter.severityBlocksNavigation(facesContext, uiComponent, facesMessage.getSeverity()))
        {
            facesContext.renderResponse();
        }
    }

    public static boolean severityBlocksSubmitForComponentId(String clientId, FacesMessage facesMessage)
    {
        ViolationSeverityInterpreter interpreter =
                ExtValContext.getContext().getViolationSeverityInterpreter();

        FacesContext facesContext = FacesContext.getCurrentInstance();
        UIComponent targetComponent = findComponent(clientId);

        return interpreter.severityBlocksSubmit(facesContext, targetComponent, facesMessage.getSeverity());
    }

    //available for add-ons - not used internally due to performance reasons
    public static boolean severityShowsIndicationForComponentId(String clientId, FacesMessage facesMessage)
    {
        ViolationSeverityInterpreter interpreter =
                ExtValContext.getContext().getViolationSeverityInterpreter();

        FacesContext facesContext = FacesContext.getCurrentInstance();
        UIComponent targetComponent = findComponent(clientId);

        return interpreter.severityShowsIndication(facesContext, targetComponent, facesMessage.getSeverity());
    }

    private static UIComponent findComponent(String clientId)
    {
        UIComponent targetComponent = null;

        if (clientId != null)
        {
            targetComponent = FacesContext.getCurrentInstance().getViewRoot().findComponent(clientId);
        }
        return targetComponent;
    }

    /**
     * @return true if component initialization for required validation is activated
     */
    public static boolean isRequiredInitializationActive()
    {
        return Boolean.TRUE.equals(ExtValContext.getContext().getGlobalProperty("mode:init:required"));
    }

    /**
     * needed for some component libs - if required initialization is used e.g. for visual indicators
     * but features like severity aware validation aren't used.
     * in such a case it's possible to use the required attribute.
     *
     * @return false to deactivate the final reset of the value of the required attribute
     */
    public static boolean isRequiredResetActivated()
    {
        return Boolean.TRUE.equals(ExtValContext.getContext().getGlobalProperty("mode:reset:required"));
    }

    public static ProjectStageName getDefaultStageName()
    {
        return createProjectStageName("Production");
    }

    public static ProjectStageName createProjectStageName(String name)
    {
        return DefaultProjectName.createProjectStageName(name);
    }

    public static boolean isExtValDeactivated()
    {
        return "true".equalsIgnoreCase(System
                .getProperty(ExtValInformation.EXTENSIONS_VALIDATOR_BASE_PACKAGE_NAME +
                    ".DEACTIVATE_ALL", "false"));
    }
}
